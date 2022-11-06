package main

import (
	"bufio"
	"context"
	_ "embed"
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"

	gm "github.com/hymkor/gmnlisp"
)

//go:embed embed.lsp
var embededLsp string

const (
	stringTarget      = "$@"
	stringFirstSource = "$<"
	stringUpdated     = "$?"
	stringPathSep     = "$/"
)

var (
	rxEmbed = regexp.MustCompile(`\$(\(.*?\)|[\<\?\/\$\@])`)

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

func funExpandString(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	s, ok := list[0].(gm.String)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	return gm.String(expandLiteral(w, s.String())), nil
}

func shouldUpdate(_list gm.Node) (bool, gm.Node, error) {
	targetNode, list, err := gm.Shift(_list)
	if err != nil {
		return false, nil, fmt.Errorf("%w: %s", err, gm.ToString(_list, gm.PRINT))
	}
	targetPath, ok := targetNode.(gm.String)
	if !ok {
		return false, nil, fmt.Errorf("%w: %s", gm.ErrExpectedString, gm.ToString(targetNode, gm.PRINT))
	}
	targetInfo, err := os.Stat(targetPath.String())
	if err != nil {
		if os.IsNotExist(err) {
			return true, list, nil
		}
		return false, nil, fmt.Errorf("%w: '%s'", err, targetPath.String())
	}
	targetStamp := targetInfo.ModTime()

	var updatedFiles gm.Node = gm.Null
	for gm.HasValue(list) {
		var sourceNode gm.Node

		sourceNode, list, err = gm.Shift(list)
		if err != nil {
			return false, nil, fmt.Errorf("%w: ..%s", err, gm.ToString(list, gm.PRINT))
		}
		sourcePath, ok := sourceNode.(gm.String)
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

func doMake(ctx context.Context, w *gm.World, depend map[gm.String][2]gm.Node, rule [2]gm.Node) (bool, error) {
	// skip first (=target)
	_, sources, err := gm.Shift(rule[0])
	if err != nil {
		return false, err
	}
	for gm.HasValue(sources) {
		var source gm.Node
		var err error

		source, sources, err = gm.Shift(sources)
		if err != nil {
			return false, fmt.Errorf("%w: %s", err, gm.ToString(sources, gm.PRINT))
		}
		sourceStr, ok := source.(gm.String)
		if !ok {
			return false, gm.ErrExpectedString
		}
		if _rule, ok := depend[sourceStr]; ok {
			if _, err := doMake(ctx, w, depend, _rule); err != nil {
				return false, err
			}
		}
	}
	isUpdate, updatedFiles, err := shouldUpdate(rule[0])
	if err != nil {
		return false, err
	}
	if !isUpdate {
		return false, nil
	}
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
		return false, err
	}
	return true, nil
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
			return nil, fmt.Errorf("%w: %s", err, gm.ToString(condAndAction, gm.PRINT))
		}
		targetNode, _, err := gm.Shift(cond)
		if err != nil {
			return nil, fmt.Errorf("%w: %s", err, gm.ToString(condAndAction, gm.PRINT))
		}
		target, ok := targetNode.(gm.String)
		if !ok {
			return nil, fmt.Errorf("%w: %s", gm.ErrExpectedString, gm.ToString(targetNode, gm.PRINT))
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
	isUpdate, err := doMake(ctx, w, depend, startRule)
	if err != nil {
		return gm.Null, err
	}
	if !isUpdate {
		fmt.Fprintf(os.Stderr, "'%s' is up to date.\n", defaultTarget)
	}
	return gm.Null, nil
}

var flagMakefile = flag.String("f", "Makefile.lsp", "Read FILE as a makefile.lsp")

var flagExecute = flag.String("e", "", "inline script")

func setupFunctions(args []string) gm.Variables {
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
		gm.NewSymbol("$"):            &gm.Function{C: 1, F: funExpandString},
		gm.NewSymbol("$$"):           cons,
		gm.NewSymbol("*args*"):       argsSeq,
		gm.NewSymbol("-d"):           &gm.Function{C: 1, F: funIsDirectory},
		gm.NewSymbol("abspath"):      &gm.Function{C: 1, F: funAbsPath},
		gm.NewSymbol("assert"):       gm.SpecialF(cmdAssert),
		gm.NewSymbol("basename"):     &gm.Function{C: 1, F: funBasename},
		gm.NewSymbol("chdir"):        &gm.Function{C: 1, F: funChdir},
		gm.NewSymbol("cp"):           &gm.Function{C: -1, F: funCopy},
		gm.NewSymbol("dir"):          &gm.Function{C: -1, F: funDir},
		gm.NewSymbol("getenv"):       &gm.Function{C: 1, F: funGetenv},
		gm.NewSymbol("getwd"):        gm.SpecialF(cmdGetwd),
		gm.NewSymbol("make"):         gm.SpecialF(cmdMake),
		gm.NewSymbol("mv"):           &gm.Function{C: -1, F: funMove},
		gm.NewSymbol("notdir"):       &gm.Function{C: 1, F: funNotDir},
		gm.NewSymbol("pathjoin"):     &gm.Function{C: -1, F: funJoinPath},
		gm.NewSymbol("q"):            &gm.Function{C: -1, F: funQuoteCommand},
		gm.NewSymbol("rm"):           &gm.Function{C: -1, F: funRemove},
		gm.NewSymbol("setenv"):       &gm.Function{C: 2, F: funSetenv},
		gm.NewSymbol("sh"):           &gm.Function{C: -1, F: funSh},
		gm.NewSymbol("sh-"):          &gm.Function{C: -1, F: funShIgnoreError},
		gm.NewSymbol("shell"):        &gm.Function{C: 1, F: funShell},
		gm.NewSymbol("stat"):         &gm.Function{C: 1, F: funStat},
		gm.NewSymbol("touch"):        &gm.Function{C: -1, F: funTouch},
		gm.NewSymbol("wildcard"):     &gm.Function{C: 1, F: funWildcard},
		gm.NewSymbol("x"):            &gm.Function{C: -1, F: funExecute},
		gm.NewSymbol("shellexecute"): &gm.Function{C: -1, F: funShellExecute},
		symbolPathSep:                gm.String(os.PathSeparator),
	}
	for i, sq := 0, argsSeq; i < 9; i++ {
		var val gm.Node = gm.Null
		if cons, ok := sq.(*gm.Cons); ok && cons != nil {
			val = cons.Car
			sq = cons.Cdr
		}
		vars[gm.NewSymbol("$"+string("123456789"[i]))] = val
	}

	if arg0, err := os.Executable(); err == nil {
		vars[gm.NewSymbol("$0")] = gm.String(arg0)
	} else {
		println(err.Error())
	}

	return vars
}

func mains(args []string) error {
	var source []byte

	ctx := context.Background()

	if *flagExecute != "" {
		source = []byte(*flagExecute)
	} else {
		fd, err := os.Open(*flagMakefile)
		if err != nil {
			return err
		}
		br := bufio.NewReader(fd)
		magicByte, err := br.Peek(1)
		if err != nil {
			return fmt.Errorf("%s: %w", *flagMakefile, err)
		}
		if magicByte[0] == '#' || magicByte[0] == '@' {
			br.ReadString('\n')
		}
		source, err = io.ReadAll(br)
		if err != nil {
			return fmt.Errorf("%s: %w", *flagMakefile, err)
		}
		fd.Close()
	}

	vars := setupFunctions(args)

	lisp := gm.New().Let(vars)
	if _, err := lisp.Interpret(ctx, embededLsp); err != nil {
		panic(err.Error())
	}

	_, err := lisp.InterpretBytes(ctx, source)
	return err
}

func main() {
	flag.Parse()
	if err := mains(flag.Args()); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
