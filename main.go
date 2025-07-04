package main

import (
	"bufio"
	"context"
	_ "embed"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"runtime"
	"strings"

	gm "github.com/hymkor/gmnlisp"
	_ "github.com/hymkor/gmnlisp/eval"
)

//go:embed embed.lsp
var embededLsp string

func setupFunctions(args []string) (gm.Variables, gm.Functions) {
	var cons gm.Node = gm.Null
	var argsSeq gm.Node = gm.Null
	for i := len(args) - 1; i >= 0; i-- {
		if name, value, ok := strings.Cut(args[i], "="); ok {
			cons = &gm.Cons{
				Car: &gm.Cons{
					Car: gm.String(name),
					Cdr: gm.String(value)},
				Cdr: cons}
		} else {
			argsSeq = &gm.Cons{Car: gm.String(args[i]), Cdr: argsSeq}
		}
	}

	executable := os.Args[0]
	if value, err := os.Executable(); err == nil {
		executable = value
	}

	vars := gm.Variables{
		gm.NewSymbol("$$"):                cons,
		gm.NewSymbol("$/"):                gm.String(os.PathSeparator),
		gm.NewSymbol("$0"):                gm.String(executable),
		gm.NewSymbol("*args*"):            argsSeq, // deprecated
		gm.NewSymbol("*argv*"):            argsSeq,
		gm.NewSymbol("*executable-name*"): gm.String(executable),
	}
	funcs := gm.Functions{
		gm.NewSymbol("$"):                      gm.Function1(funExpandString),
		gm.NewSymbol("abspath"):                gm.Function1(funAbsPath),
		gm.NewSymbol("ansi-to-utf8"):           gm.Function1(funAnsiToUtf8),
		gm.NewSymbol("assert"):                 gm.SpecialF(cmdAssert),
		gm.NewSymbol("basename"):               gm.Function1(funBasename),
		gm.NewSymbol("chdir"):                  gm.Function1(funChdir),
		gm.NewSymbol("cp"):                     &gm.Function{C: -1, F: funCopy},
		gm.NewSymbol("dir"):                    &gm.Function{C: -1, F: funDir},
		gm.NewSymbol("executable-not-found-p"): gm.Function1(funExecutableNotFoundP),
		gm.NewSymbol("exit-code"):              gm.Function1(funExitCode),
		gm.NewSymbol("exit-error-p"):           gm.Function1(funExitErrorP),
		gm.NewSymbol("getenv"):                 gm.Function1(funGetenv),
		gm.NewSymbol("getwd"):                  gm.SpecialF(cmdGetwd),
		gm.NewSymbol("join-path"):              &gm.Function{C: -1, F: funJoinPath},
		gm.NewSymbol("make"):                   gm.SpecialF(cmdMake),
		gm.NewSymbol("match"):                  gm.Function2(funMatch),
		gm.NewSymbol("mv"):                     &gm.Function{C: -1, F: funMove},
		gm.NewSymbol("notdir"):                 gm.Function1(funNotDir),
		gm.NewSymbol("rm"):                     &gm.Function{C: -1, F: funRemove},
		gm.NewSymbol("setenv"):                 gm.Function2(funSetenv),
		gm.NewSymbol("sh"):                     &gm.Function{C: -1, F: funSh},
		gm.NewSymbol("shell"):                  gm.Function1(funShell),
		gm.NewSymbol("shellexecute"):           &gm.Function{C: -1, F: funShellExecute},
		gm.NewSymbol("spawn"):                  &gm.Function{C: -1, F: funExecute},
		gm.NewSymbol("stat"):                   gm.Function1(funStat),
		gm.NewSymbol("string-fields"):          gm.Function1(funFields),
		gm.NewSymbol("touch"):                  &gm.Function{C: -1, F: funTouch},
		gm.NewSymbol("wildcard"):               &gm.Function{C: -1, F: funWildcard},
	}
	for i, sq := 0, argsSeq; i < 9; i++ {
		var val gm.Node = gm.Null
		if cons, ok := sq.(*gm.Cons); ok && cons != nil {
			val = cons.Car
			sq = cons.Cdr
		}
		vars[gm.NewSymbol("$"+string("123456789"[i]))] = val
	}

	return vars, funcs
}

func openFiles(filenames string) (*os.File, error) {
	errs := []error{}
	for {
		var filename1 string
		var ok bool
		filename1, filenames, ok = strings.Cut(filenames, ",")
		if filename1 != "" {
			fd, err := os.Open(filename1)
			if err == nil {
				return fd, nil
			}
			errs = append(errs, err)
		}
		if !ok {
			if len(errs) >= 2 {
				return nil, errors.Join(errs...)
			}
			if len(errs) == 1 {
				return nil, errs[0]
			}
			return nil, errors.New("no filenames specified")
		}
	}
}

var (
	flagVersion  = flag.Bool("version", false, "show version")
	flagMakefile = flag.String("f", "Makefile.lsp,smake.lsp", "Read comma-separated FILES as rule files; the first existing file is used")
	flagExecute  = flag.String("e", "", "inline script")
)

var version string = "snapshot"

func mains(args []string) error {
	var source []byte

	if *flagVersion {
		fmt.Printf("smake %s-%s-%s by %s\n",
			version,
			runtime.GOOS,
			runtime.GOARCH,
			runtime.Version())
		return nil
	}

	ctx := context.Background()

	if *flagExecute != "" {
		source = []byte(*flagExecute)
	} else {
		fd, err := openFiles(*flagMakefile)
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

	vars, functions := setupFunctions(args)

	w := gm.New().Let(vars).Flet(functions)
	if _, err := w.Interpret(ctx, embededLsp); err != nil {
		return err
	}

	_, err := w.InterpretBytes(ctx, source)
	return err
}

func main() {
	flag.Parse()
	if err := mains(flag.Args()); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
