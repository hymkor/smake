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
			return s
		}
		newString, ok := value.(gm.String)
		if !ok {
			return s
		}
		return newString.String()
	})
}

func funEcho(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	stdout := w.Stdout()
	for i, s := range list {
		if i > 0 {
			stdout.Write([]byte{' '})
		}
		s.PrintTo(stdout, gm.PRINC)
	}
	fmt.Fprintln(stdout)
	return gm.Null, nil
}

func funExecute(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	stdout := w.Stdout()

	argv := make([]string, len(list))
	for i, value := range list {
		var buffer strings.Builder
		value.PrintTo(&buffer, gm.PRINC)
		argv[i] = buffer.String()
		if i > 0 {
			stdout.Write([]byte{' '})
		}
		io.WriteString(stdout, argv[i])
	}
	fmt.Fprintln(stdout)

	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdout = w.Stdout()
	cmd.Stderr = w.Errout()
	cmd.Stdin = os.Stdin // w.Stdin()
	return gm.Null, cmd.Run()
}

func funUpdate(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
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
		return false, gm.ErrExpectedString
	}
	// println("targetPath=", targetPath.String())
	targetInfo, err := os.Stat(targetPath.String())
	if err != nil {
		if os.IsNotExist(err) {
			return true, nil
		}
		return false, err
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
		_, err = gm.Progn(ctx, w, rule[1])
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
	return gm.Null, doMake(ctx, w, depend, depend[defaultTarget])
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
			gm.NewSymbol("update"): &gm.Function{C: -1, F: funUpdate},
			gm.NewSymbol("make"):   gm.SpecialF(cmdMake),
			gm.NewSymbol("x"):      &gm.Function{C: -1, F: funExecute},
			gm.NewSymbol("echo"):   &gm.Function{C: -1, F: funEcho},
			gm.NewSymbol("target"): gm.String(target),
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
