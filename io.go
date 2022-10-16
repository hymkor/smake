package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	gm "github.com/hymkor/gmnlisp"

	"github.com/hymkor/smake/internal/file"
)

func funJoinPath(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	paths := make([]string, 0, len(list))
	for _, node := range list {
		str, ok := node.(gm.StringTypes)
		if !ok {
			return nil, fmt.Errorf("%w: %s", gm.ErrExpectedString, gm.ToString(node, gm.PRINT))
		}
		paths = append(paths, str.String())
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
	_key, ok := list[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	key := _key.String()

	_value, ok := list[1].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	value := _value.String()

	fmt.Fprintf(w.Errout(), "setenv \"%s=%s\"\n", key, value)
	return gm.Null, os.Setenv(key, value)
}

func cmdEnv(ctx context.Context, w *gm.World, list gm.Node) (gm.Node, error) {
	var vars gm.Node
	var err error

	vars, list, err = gm.Shift(list)
	if err != nil {
		return nil, err
	}
	backup := map[string]string{}
	remove := map[string]struct{}{}

	for gm.HasValue(vars) {
		var set gm.Node

		set, vars, err = gm.Shift(vars)
		if err != nil {
			return nil, err
		}

		var _name gm.Node
		_name, set, err = gm.Shift(set)
		if err != nil {
			return nil, err
		}
		var name string
		if nameStr, ok := _name.(gm.StringTypes); ok {
			name = nameStr.String()
		} else {
			return nil, gm.ErrExpectedString
		}

		var _value gm.Node
		_value, set, err = w.ShiftAndEvalCar(ctx, set)
		if err != nil {
			return nil, err
		}
		var value string
		if valueStr, ok := _value.(gm.StringTypes); ok {
			value = valueStr.String()
		} else {
			return nil, gm.ErrExpectedString
		}

		if gm.HasValue(set) {
			return nil, gm.ErrTooManyArguments
		}

		if orig, ok := os.LookupEnv(name); ok {
			backup[name] = orig
		} else {
			remove[name] = struct{}{}
		}
		if err := os.Setenv(name, value); err != nil {
			return nil, err
		}
		fmt.Fprintf(os.Stderr, "setenv \"%s=%s\"\n", name, value)
	}
	rc, err := gm.Progn(ctx, w, list)

	for name := range remove {
		os.Unsetenv(name)
		fmt.Fprintf(os.Stderr, "unsetenv \"%s\"\n", name)
	}

	for name, value := range backup {
		os.Setenv(name, value)
		fmt.Fprintf(os.Stderr, "setenv(restore) \"%s=%s\"\n", name, value)
	}

	return rc, err
}

func funRemove(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	for _, fnNode := range list {
		fnStr, ok := fnNode.(gm.StringTypes)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		fname := fnStr.String()
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
		fname := fnStr.String()
		fd, err := os.OpenFile(fname, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0666)
		if err == nil {
			if err = fd.Close(); err != nil {
				return nil, fmt.Errorf("close %s: %w", fname, err)
			}
			os.Chtimes(fname, stamp, stamp)
			fmt.Fprintf(os.Stderr, "touch \"%s\"\n", fname)
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
		io.WriteString(stdout, gm.ToString(s, gm.PRINC))
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

	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd
}

func funExecute(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	cmd := nodesToCommand(ctx, w, list, w.Errout())
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return gm.Null, cmd.Run()
}

func funSh(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	s, ok := list[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	cmdline := s.String()
	fmt.Fprintln(os.Stderr, cmdline)
	cmd := newShell(cmdline)
	// cmd.Stdout = w.Stdout()
	// cmd.Stderr = w.Errout()
	return gm.Null, cmd.Run()
}

func funShIgnoreError(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	rv, err := funSh(ctx, w, list)
	var ignoreType *exec.ExitError
	if errors.As(err, &ignoreType) {
		// fmt.Printf("%+T\n", err)
		return rv, nil
	}
	return rv, err
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
	s := string(output)
	s = strings.ReplaceAll(s, "\r", "") // CRLF -> LF
	s = strings.TrimSpace(s)
	return gm.String(s), nil
}

func cmdPushd(ctx context.Context, w *gm.World, node gm.Node) (gm.Node, error) {
	dirNode, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	dir, ok := dirNode.(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	curDir, err := os.Getwd()
	if err != nil {
		return nil, err
	}
	defer func() {
		fmt.Fprintf(os.Stderr, "cd \"%s\"\n", curDir)
		os.Chdir(curDir)
	}()

	_dir := dir.String()
	fmt.Fprintf(os.Stderr, "cd \"%s\"\n", _dir)
	err = os.Chdir(_dir)
	if err != nil {
		return nil, err
	}
	return gm.Progn(ctx, w, node)
}

func funCopy(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	return copyOrMove(list, "cp", func(s, d string) error {
		return file.Copy(s, d, false)
	})
}

func funMove(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	return copyOrMove(list, "mv", func(s, d string) error {
		return file.Move(s, d)
	})
}

func copyOrMove(list []gm.Node, msg string, f func(s, d string) error) (gm.Node, error) {
	if len(list) < 2 {
		return nil, gm.ErrTooFewArguments
	}
	_destinate, ok := list[len(list)-1].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	destinate := _destinate.String()

	isDir := false
	if fileInfo, err := os.Stat(destinate); err == nil && fileInfo.IsDir() {
		isDir = true
	} else {
		if len(list) >= 3 {
			return nil, fmt.Errorf("invalid destination: %s", destinate)
		}
	}

	for _, s := range list[:len(list)-1] {
		_source, ok := s.(gm.StringTypes)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		source := _source.String()

		var newFile string
		if isDir {
			newFile = filepath.Join(destinate, filepath.Base(source))
		} else {
			newFile = destinate
		}
		fmt.Printf("%s \"%s\" \"%s\"\n", msg, source, newFile)
		err := f(source, newFile)
		if err != nil {
			return gm.Null, err
		}
	}
	return gm.Null, nil
}
