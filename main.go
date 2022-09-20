package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"time"

	gm "github.com/hymkor/gmnlisp"
)

const (
	stringTarget      = "$@"
	stringFirstSource = "$<"
)

var (
	rxEmbed = regexp.MustCompile(`\$\(.*?\)`)

	errExpectedVector = errors.New("Expected Vector")
	symbolTarget      = gm.NewSymbol(stringTarget)
	symbolFirstSource = gm.NewSymbol(stringFirstSource)
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

func expandLiteral(w *gm.World, s string) string {
	if val, err := w.Get(symbolTarget); err == nil {
		s = strings.ReplaceAll(s, stringTarget, gm.ToString(val, gm.PRINC))
	}
	if val, err := w.Get(symbolFirstSource); err == nil {
		s = strings.ReplaceAll(s, stringFirstSource, gm.ToString(val, gm.PRINC))
	}
	dic := dollar(w)
	return rxEmbed.ReplaceAllStringFunc(s, func(s string) string {
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
		} else if newString, ok := value.(gm.String); ok {
			return newString.String()
		}
		return ""
	})
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

	return exec.CommandContext(ctx, argv[0], argv[1:]...)
}

func funExecute(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	cmd := nodesToCommand(ctx, w, list, w.Errout())
	cmd.Stdout = w.Stdout()
	cmd.Stderr = w.Errout()
	cmd.Stdin = os.Stdin // w.Stdin()
	return gm.Null, cmd.Run()
}

func funQuoteCommand(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	cmd := nodesToCommand(ctx, w, list, io.Discard)
	cmd.Stderr = w.Errout()
	cmd.Stdin = os.Stdin // w.Stdin()
	output, err := cmd.Output()
	if err != nil {
		return gm.Null, err
	}
	return gm.String(strings.TrimSpace(string(output))), nil
}

func cmdWithRedirectOut(ctx context.Context, w *gm.World, node gm.Node) (gm.Node, error) {

	return withRedirect(ctx, w, node, os.Create)
}

func cmdWithRedirectOutAppend(ctx context.Context, w *gm.World, node gm.Node) (gm.Node, error) {
	return withRedirect(ctx, w, node, func(fn string) (*os.File, error) {
		return os.OpenFile(fn, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0644)
	})
}

func withRedirect(ctx context.Context, w *gm.World, node gm.Node, f func(string) (*os.File, error)) (gm.Node, error) {
	_outputPath, prog, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	outputPath, ok := _outputPath.(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	fd, err := f(expandLiteral(w, outputPath.String()))
	if err != nil {
		return nil, err
	}
	defer func() {
		fd.Sync()
		fd.Close()
	}()

	orgStdout := w.Stdout()
	defer w.SetStdout(orgStdout)

	w.SetStdout(fd)
	return gm.Progn(ctx, w, prog)
}

func funRule(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	var result []gm.Node
	for _, _name := range list {
		name, ok := _name.(gm.StringTypes)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		resultOne := expandLiteral(w, name.String())
		result = append(result, gm.String(resultOne))
	}
	return gm.List(result...), nil
}

func shouldUpdate(list gm.Node) (bool, error) {
	targetNode, list, err := gm.Shift(list)
	if err != nil {
		return false, fmt.Errorf("shouldUpdate(1): %w", err)
	}
	targetPath, ok := targetNode.(gm.StringTypes)
	if !ok {
		return false, fmt.Errorf("%s: %w", gm.ToString(targetNode, gm.PRINT), gm.ErrExpectedString)
	}
	targetInfo, err := os.Stat(targetPath.String())
	if err != nil {
		if os.IsNotExist(err) {
			return true, nil
		}
		return false, fmt.Errorf("os.Stat('%s'): %w", targetPath.String(), err)
	}
	targetStamp := targetInfo.ModTime()

	for gm.HasValue(list) {
		var sourceNode gm.Node

		sourceNode, list, err = gm.Shift(list)
		if err != nil {
			return false, fmt.Errorf("shouldUpdate(2): %w", err)
		}
		sourcePath, ok := sourceNode.(gm.StringTypes)
		if !ok {
			return false, gm.ErrExpectedString
		}
		sourceInfo, err := os.Stat(sourcePath.String())
		if err != nil {
			continue
		}
		sourceStamp := sourceInfo.ModTime()
		if sourceStamp.After(targetStamp) {
			return true, nil
		}
	}
	return false, nil
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
	isUpdate, err := shouldUpdate(rule[0])
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
	if target, err := w.Get(gm.NewSymbol("target")); err == nil {
		defaultTarget = target.(gm.String)
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

	lisp := gm.New().Let(
		gm.Variables{
			gm.NewSymbol("rule"):  &gm.Function{C: -1, F: funRule},
			gm.NewSymbol("make"):  gm.SpecialF(cmdMake),
			gm.NewSymbol("x"):     &gm.Function{C: -1, F: funExecute},
			gm.NewSymbol("echo"):  &gm.Function{C: -1, F: funEcho},
			gm.NewSymbol("q"):     &gm.Function{C: -1, F: funQuoteCommand},
			gm.NewSymbol("1>"):    gm.SpecialF(cmdWithRedirectOut),
			gm.NewSymbol("1>>"):   gm.SpecialF(cmdWithRedirectOutAppend),
			gm.NewSymbol("touch"): &gm.Function{C: -1, F: funTouch},
			gm.NewSymbol("rm"):    &gm.Function{C: -1, F: funRemove},
		})

	var cons gm.Node = gm.Null
	for _, s := range args {
		if name, value, ok := strings.Cut(s, "="); ok {
			cons = &gm.Cons{
				Car: &gm.Cons{
					Car: gm.String(name),
					Cdr: gm.String(value)},
				Cdr: cons}
		} else {
			lisp = lisp.Let(&gm.Pair{
				Key:   gm.NewSymbol("target"),
				Value: gm.String(s)})
		}
	}
	lisp = lisp.Let(&gm.Pair{
		Key:   gm.NewSymbol("$"),
		Value: cons})

	_, err = lisp.InterpretBytes(ctx, source)
	return err
}

func main() {
	if err := mains(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
