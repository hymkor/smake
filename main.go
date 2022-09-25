package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	gm "github.com/hymkor/gmnlisp"
)

const (
	stringTarget      = "$@"
	stringFirstSource = "$<"
	stringUpdated     = "$?"
	stringPathSep     = "$/"
)

var (
	rxEmbed = regexp.MustCompile(`\$(\(.*?\)|[\<\?\/\$\@])`)

	errExpectedVector = errors.New("Expected Vector")
	symbolTarget      = gm.NewSymbol(stringTarget)
	symbolFirstSource = gm.NewSymbol(stringFirstSource)
	symbolUpdated     = gm.NewSymbol(stringUpdated)
	symbolPathSep     = gm.NewSymbol(stringPathSep)
)

func dollar(w *gm.World) func(string) (string, bool, error) {
	assoc, err := w.Get(gm.NewSymbol("$"))
	if err != nil {
		return func(string) (string, bool, error) {
			return "", false, nil
		}
	}
	return func(s string) (string, bool, error) {
		pair, err := gm.Assoc(gm.String(s), assoc)
		if err != nil {
			return "", false, err
		}
		if gm.IsNull(pair) {
			return "", false, nil
		}
		cons, ok := pair.(*gm.Cons)
		if !ok {
			return "", false, gm.ErrExpectedCons
		}
		valueStr, ok := cons.Cdr.(gm.String)
		if !ok {
			return "", false, gm.ErrExpectedString
		}
		return valueStr.String(), true, nil
	}
}

func joinSequence(w *gm.World, node gm.Node) string {
	var buffer strings.Builder
	if _, ok := node.(gm.Sequence); ok {
		gm.SeqEach(node, func(value gm.Node) error {
			if buffer.Len() > 0 {
				buffer.WriteByte(' ')
			}
			value.PrintTo(&buffer, gm.PRINC)
			return nil
		})
	} else {
		node.PrintTo(&buffer, gm.PRINC)
	}
	return buffer.String()
}

func expandLiteral(w *gm.World, s string) string {
	dic := dollar(w)
	return rxEmbed.ReplaceAllStringFunc(s, func(s string) string {
		if len(s) == 2 {
			switch s[1] {
			case '@':
				if val, err := w.Get(symbolTarget); err == nil {
					return gm.ToString(val, gm.PRINC)
				}
			case '<':
				if val, err := w.Get(symbolFirstSource); err == nil {
					return gm.ToString(val, gm.PRINC)
				}
			case '?':
				if list, err := w.Get(symbolUpdated); err == nil {
					return joinSequence(w, list)
				}
			case '/':
				return string(os.PathSeparator)
			case '$':
				return "$"
			}
			return s
		} else {
			key := s[2 : len(s)-1]
			//println("replace:", key)
			value, err := w.Get(gm.NewSymbol(key))
			if err != nil {
				if value, ok := os.LookupEnv(key); ok {
					return value
				}
				value, ok, err := dic(key)
				if err != nil {
					println(err.Error())
				} else if ok {
					return value
				}
			}
			return gm.ToString(value, gm.PRINC)
		}
	})
}

func expandLiteralNodes(w *gm.World, node gm.Node) (gm.Node, error) {
	var result gm.ListBuilder
	for gm.HasValue(node) {
		var car gm.Node
		var err error

		car, node, err = gm.Shift(node)
		if err != nil {
			return nil, err
		}
		s, ok := car.(gm.StringTypes)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		result.Add(gm.String(expandLiteral(w, s.String())))
	}
	return result.Sequence(), nil
}

func funJoinPath(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	paths := make([]string, 0, len(list))
	for _, node := range list {
		str, ok := node.(gm.StringTypes)
		if !ok {
			return nil, fmt.Errorf("%w: %s", gm.ErrExpectedString, gm.ToString(node, gm.PRINT))
		}
		paths = append(paths, expandLiteral(w, str.String()))
	}
	return gm.String(filepath.Join(paths...)), nil
}

func cmdAssert(ctx context.Context, w *gm.World, node gm.Node) (gm.Node, error) {
	value, _, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return gm.Null, err
	}
	if gm.HasValue(value) {
		return gm.Null, nil
	}
	return gm.Null, fmt.Errorf("Assertion failed: %s", gm.ToString(node, gm.PRINT))
}

func funGetenv(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	key, ok := list[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	value, ok := os.LookupEnv(key.String())
	if !ok {
		return gm.Null, nil
	}
	return gm.String(value), nil
}

func funSetenv(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	key, ok := list[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	value, ok := list[1].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	return gm.Null, os.Setenv(key.String(), value.String())
}

func funRemove(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	for _, fnNode := range list {
		fnStr, ok := fnNode.(gm.StringTypes)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		fname := expandLiteral(w, fnStr.String())
		if err := os.Remove(fname); err == nil {
			fmt.Fprintf(w.Errout(), "rm \"%s\"\n", fname)
		}
	}
	return gm.Null, nil
}

func funTouch(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	stamp := time.Now()
	for _, fnNode := range list {
		fnStr, ok := fnNode.(gm.StringTypes)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		fname := expandLiteral(w, fnStr.String())
		fd, err := os.OpenFile(fname, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0666)
		if err == nil {
			if err = fd.Close(); err != nil {
				return nil, fmt.Errorf("close %s: %w", fname, err)
			}
			os.Chtimes(fname, stamp, stamp)
		} else {
			return nil, fmt.Errorf("open %s: %w", fname, err)
		}
	}
	return gm.Null, nil
}

func funEcho(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	stdout := w.Stdout()
	for i, s := range list {
		if i > 0 {
			stdout.Write([]byte{' '})
		}
		io.WriteString(stdout, expandLiteral(w, gm.ToString(s, gm.PRINC)))
	}
	fmt.Fprintln(stdout)
	return gm.Null, nil
}

func nodesToCommand(ctx context.Context, w *gm.World, list []gm.Node, out io.Writer) *exec.Cmd {
	argv := make([]string, len(list))
	for i, value := range list {
		argv[i] = expandLiteral(w, gm.ToString(value, gm.PRINC))
		if i > 0 {
			out.Write([]byte{' '})
		}
		io.WriteString(out, argv[i])
	}
	fmt.Fprintln(out)

	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd
}

func funExecute(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	cmd := nodesToCommand(ctx, w, list, w.Errout())
	cmd.Stdout = w.Stdout()
	cmd.Stderr = w.Errout()
	return gm.Null, cmd.Run()
}

func funQuote(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	s, ok := list[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	cmdline := expandLiteral(w, s.String())
	cmd := newShell(cmdline)
	cmd.Stdout = nil
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}
	return gm.String(strings.TrimSpace(string(output))), nil
}

func funShell(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	s, ok := list[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	cmdline := expandLiteral(w, s.String())
	fmt.Fprintln(os.Stderr, cmdline)
	cmd := newShell(cmdline)
	// cmd.Stdout = w.Stdout()
	// cmd.Stderr = w.Errout()
	return gm.Null, cmd.Run()
}

func funQuoteCommand(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	cmd := nodesToCommand(ctx, w, list, io.Discard)
	cmd.Stdout = nil
	cmd.Stderr = w.Errout()
	cmd.Stdin = os.Stdin // w.Stdin()
	output, err := cmd.Output()
	if err != nil {
		return gm.Null, err
	}
	return gm.String(strings.TrimSpace(string(output))), nil
}

func shouldUpdate(list gm.Node) (bool, gm.Node, error) {
	targetNode, list, err := gm.Shift(list)
	if err != nil {
		return false, nil, fmt.Errorf("shouldUpdate(1): %w", err)
	}
	targetPath, ok := targetNode.(gm.StringTypes)
	if !ok {
		return false, nil, fmt.Errorf("%s: %w", gm.ToString(targetNode, gm.PRINT), gm.ErrExpectedString)
	}
	targetInfo, err := os.Stat(targetPath.String())
	if err != nil {
		if os.IsNotExist(err) {
			return true, list, nil
		}
		return false, nil, fmt.Errorf("os.Stat('%s'): %w", targetPath.String(), err)
	}
	targetStamp := targetInfo.ModTime()

	var updatedFiles gm.Node = gm.Null
	for gm.HasValue(list) {
		var sourceNode gm.Node

		sourceNode, list, err = gm.Shift(list)
		if err != nil {
			return false, nil, fmt.Errorf("shouldUpdate(2): %w", err)
		}
		sourcePath, ok := sourceNode.(gm.StringTypes)
		if !ok {
			return false, nil, gm.ErrExpectedString
		}
		sourceInfo, err := os.Stat(sourcePath.String())
		if err != nil {
			continue
		}
		sourceStamp := sourceInfo.ModTime()
		if sourceStamp.After(targetStamp) {
			updatedFiles = &gm.Cons{
				Car: sourceNode,
				Cdr: updatedFiles,
			}
		}
	}
	return gm.HasValue(updatedFiles), updatedFiles, nil
}

func doMake(ctx context.Context, w *gm.World, depend map[gm.String][2]gm.Node, rule [2]gm.Node) error {
	// skip first (=target)
	_, sources, err := gm.Shift(rule[0])
	if err != nil {
		return err
	}
	for gm.HasValue(sources) {
		var source gm.Node
		var err error

		source, sources, err = gm.Shift(sources)
		if err != nil {
			return fmt.Errorf("doMake(1): %w", err)
		}
		sourceStr, ok := source.(gm.String)
		if !ok {
			return gm.ErrExpectedString
		}
		if _rule, ok := depend[sourceStr]; ok {
			if err := doMake(ctx, w, depend, _rule); err != nil {
				return fmt.Errorf("doMake(2): %w", err)
			}
		}
	}
	isUpdate, updatedFiles, err := shouldUpdate(rule[0])
	if err != nil {
		return err
	}
	if isUpdate {
		var target gm.Node = gm.String("")
		var firstSource gm.Node = gm.String("")
		if cons1, ok := rule[0].(*gm.Cons); ok {
			target = cons1.Car
			if cons2, ok := cons1.Cdr.(*gm.Cons); ok {
				firstSource = cons2.Car
			}
		}
		newWorld := w.Let(
			gm.Variables{
				symbolTarget:      target,
				symbolFirstSource: firstSource,
				symbolUpdated:     updatedFiles,
			})
		_, err = gm.Progn(ctx, newWorld, rule[1])
		if err != nil {
			return err
		}
	}
	return nil
}

func cmdMake(ctx context.Context, w *gm.World, node gm.Node) (gm.Node, error) {
	var defaultTarget gm.String

	_defaultTarget, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	if val, ok := _defaultTarget.(gm.String); ok {
		defaultTarget = val
	}

	depend := map[gm.String][2]gm.Node{}

	for gm.HasValue(node) {
		var condAndAction gm.Node
		var err error

		condAndAction, node, err = gm.Shift(node)
		if err != nil {
			return nil, err
		}
		cond, action, err := w.ShiftAndEvalCar(ctx, condAndAction)
		if err != nil {
			return nil, fmt.Errorf("cmdMake(1): %w", err)
		}
		// expand $(...)
		cond, err = expandLiteralNodes(w, cond)
		if err != nil {
			return nil, err
		}
		targetNode, _, err := gm.Shift(cond)
		if err != nil {
			return nil, fmt.Errorf("cmdMake(2): %w", err)
		}
		target, ok := targetNode.(gm.String)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		if defaultTarget == "" {
			defaultTarget = target
		}
		depend[target] = [...]gm.Node{cond, action}
	}
	startRule, ok := depend[defaultTarget]
	if !ok {
		return nil, fmt.Errorf("*** No rule to make target '%s'.  Stop.", defaultTarget)
	}
	return gm.Null, doMake(ctx, w, depend, startRule)
}

func mains(args []string) error {
	ctx := context.Background()

	source, err := os.ReadFile("Makefile.lsp")
	if err != nil {
		return err
	}

	var cons gm.Node = gm.Null
	var argsList gm.ListBuilder
	for _, s := range args {
		if name, value, ok := strings.Cut(s, "="); ok {
			cons = &gm.Cons{
				Car: &gm.Cons{
					Car: gm.String(name),
					Cdr: gm.String(value)},
				Cdr: cons}
		} else {
			argsList.Add(gm.String(s))
		}
	}
	argsSeq := argsList.Sequence()

	vars := gm.Variables{
		gm.NewSymbol("abspath"):  &gm.Function{C: 1, F: funAbsPath},
		gm.NewSymbol("assert"):   gm.SpecialF(cmdAssert),
		gm.NewSymbol("echo"):     &gm.Function{C: -1, F: funEcho},
		gm.NewSymbol("getenv"):   &gm.Function{C: 1, F: funGetenv},
		gm.NewSymbol("glob"):     &gm.Function{C: 1, F: funWildcard},
		gm.NewSymbol("wildcard"): &gm.Function{C: 1, F: funWildcard},
		gm.NewSymbol("make"):     gm.SpecialF(cmdMake),
		gm.NewSymbol("notdir"):   &gm.Function{C: 1, F: funNotDir},
		gm.NewSymbol("pathjoin"): &gm.Function{C: -1, F: funJoinPath},
		gm.NewSymbol("q"):        &gm.Function{C: -1, F: funQuoteCommand},
		gm.NewSymbol("qs"):       &gm.Function{C: 1, F: funQuote},
		gm.NewSymbol("rm"):       &gm.Function{C: -1, F: funRemove},
		gm.NewSymbol("setenv"):   &gm.Function{C: 2, F: funSetenv},
		gm.NewSymbol("sh"):       &gm.Function{C: 1, F: funShell},
		gm.NewSymbol("touch"):    &gm.Function{C: -1, F: funTouch},
		gm.NewSymbol("x"):        &gm.Function{C: -1, F: funExecute},
		gm.NewSymbol("*args*"):   argsSeq,
		gm.NewSymbol("$"):        cons,
		symbolPathSep:            gm.String(os.PathSeparator),
	}
	for i, sq := 0, argsSeq; i < 9; i++ {
		var val gm.Node = gm.Null
		if cons, ok := sq.(*gm.Cons); ok && cons != nil {
			val = cons.Car
			sq = cons.Cdr
		}
		vars[gm.NewSymbol("$"+string("123456789"[i]))] = val
	}
	lisp := gm.New().Let(vars)

	_, err = lisp.InterpretBytes(ctx, source)
	return err
}

func main() {
	if err := mains(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
