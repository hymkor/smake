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

var (
	rxEmbed = regexp.MustCompile(`\$\(.*?\)`)

	errExpectedVector = errors.New("Expected Vector")
)

func expandLiteral(w *gm.World, s string) string {
	//println(s)
	return rxEmbed.ReplaceAllStringFunc(s, func(s string) string {
		key := s[2 : len(s)-1]
		//println("replace:", key)
		value, err := w.Get(gm.NewSymbol(key))
		if err != nil {
			if value, ok := os.LookupEnv(key); ok {
				return value
			}
			return s
		}
		newString, ok := value.(gm.String)
		if !ok {
			return s
		}
		return newString.String()
	})
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
		argv[i] = gm.ToString(value, gm.PRINC)
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
	fd, err := f(outputPath.String())
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
	return gm.Vector(result), nil
}

func shouldUpdate(list gm.Vector) (bool, error) {
	if len(list) < 1 {
		return false, gm.ErrTooFewArguments
	}
	targetPath, ok := list[0].(gm.StringTypes)
	if !ok {
		return false, fmt.Errorf("%s: %w", gm.ToString(list[0], gm.PRINT), gm.ErrExpectedString)
	}
	targetInfo, err := os.Stat(targetPath.String())
	if err != nil {
		if os.IsNotExist(err) {
			return true, nil
		}
		return false, fmt.Errorf("os.Stat('%s'): %w", targetPath.String(), err)
	}
	targetStamp := targetInfo.ModTime()

	for _, _sourcePath := range list[1:] {
		sourcePath, ok := _sourcePath.(gm.StringTypes)
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
	sources, ok := rule[0].(gm.Vector)
	if !ok {
		return errExpectedVector
	}
	for _, source := range sources[1:] {
		sourceStr, ok := source.(gm.String)
		if !ok {
			return gm.ErrExpectedString
		}
		if _rule, ok := depend[sourceStr]; ok {
			if err := doMake(ctx, w, depend, _rule); err != nil {
				return err
			}
		}
	}
	isUpdate, err := shouldUpdate(sources)
	if err != nil {
		return err
	}
	if isUpdate {
		var firstSource gm.Node = gm.String("")
		if len(sources) >= 2 {
			firstSource = sources[1]
		}
		newWorld := w.Let(
			gm.Variables{
				gm.NewSymbol("$@"): sources[0],
				gm.NewSymbol("$<"): firstSource,
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
			return nil, err
		}
		condVector, ok := cond.(gm.Vector)
		if !ok || len(condVector) < 1 {
			return nil, errExpectedVector
		}
		target, ok := condVector[0].(gm.String)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		if defaultTarget == "" {
			defaultTarget = target
		}
		depend[target] = [...]gm.Node{condVector, action}
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

	var target string
	if len(args) >= 1 {
		target = args[0]
	}

	lisp := gm.New().Let(
		gm.Variables{
			gm.NewSymbol("rule"):   &gm.Function{C: -1, F: funRule},
			gm.NewSymbol("make"):   gm.SpecialF(cmdMake),
			gm.NewSymbol("x"):      &gm.Function{C: -1, F: funExecute},
			gm.NewSymbol("echo"):   &gm.Function{C: -1, F: funEcho},
			gm.NewSymbol("target"): gm.String(target),
			gm.NewSymbol("q"):      &gm.Function{C: -1, F: funQuoteCommand},
			gm.NewSymbol("1>"):     gm.SpecialF(cmdWithRedirectOut),
			gm.NewSymbol("1>>"):    gm.SpecialF(cmdWithRedirectOutAppend),
			gm.NewSymbol("touch"):  &gm.Function{C: -1, F: funTouch},
		})

	_, err = lisp.InterpretBytes(ctx, source)
	return err
}

func main() {
	if err := mains(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
