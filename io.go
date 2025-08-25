package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	gm "github.com/hymkor/gmnlisp"

	"github.com/hymkor/smake/internal/file"
)

func cmdGetwd(ctx context.Context, w *gm.World, _ gm.Node) (gm.Node, error) {
	wd, err := os.Getwd()
	return gm.String(wd), err
}

func funChdir(ctx context.Context, w *gm.World, arg gm.Node) (gm.Node, error) {
	wd, err := gm.ExpectClass[gm.String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	dir := wd.String()
	fmt.Fprintf(w.Errout(), "chdir \"%s\"\n", dir)
	return gm.Null, os.Chdir(dir)
}

func funJoinPath(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	paths := make([]string, 0, len(list))
	for _, node := range list {
		str, err := gm.ExpectClass[gm.String](ctx, w, node)
		if err != nil {
			return nil, err
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
	if gm.IsSome(value) {
		return gm.Null, nil
	}
	return gm.Null, fmt.Errorf("Assertion failed: %#v", node)
}

func funGetenv(ctx context.Context, w *gm.World, arg gm.Node) (gm.Node, error) {
	key, err := gm.ExpectClass[gm.String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	value, ok := os.LookupEnv(key.String())
	if !ok {
		return gm.Null, nil
	}
	return gm.String(value), nil
}

func funSetenv(ctx context.Context, w *gm.World, left, right gm.Node) (gm.Node, error) {
	_key, err := gm.ExpectClass[gm.String](ctx, w, left)
	if err != nil {
		return nil, err
	}
	key := _key.String()

	if gm.IsNull(right) {
		fmt.Fprintf(w.Errout(), "unsetenv \"%s\"\n", key)
		return gm.Null, os.Unsetenv(key)
	}
	_value, err := gm.ExpectClass[gm.String](ctx, w, right)
	if err != nil {
		return nil, err
	}
	value := _value.String()

	fmt.Fprintf(w.Errout(), "setenv \"%s=%s\"\n", key, value)
	return gm.Null, os.Setenv(key, value)
}

func funRemove(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	for _, fnNode := range list {
		fnStr, err := gm.ExpectClass[gm.String](ctx, w, fnNode)
		if err != nil {
			return nil, err
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
		fnStr, err := gm.ExpectClass[gm.String](ctx, w, fnNode)
		if err != nil {
			return nil, err
		}
		fname := fnStr.String()
		fd, err := os.OpenFile(fname, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0666)
		if err == nil {
			if err = fd.Close(); err != nil {
				return nil, fmt.Errorf("close %s: %w", fname, err)
			}
			os.Chtimes(fname, stamp, stamp)
			fmt.Fprintf(w.Errout(), "touch \"%s\"\n", fname)
		} else {
			return nil, fmt.Errorf("open %s: %w", fname, err)
		}
	}
	return gm.Null, nil
}

func getRawWriter(w io.Writer) io.Writer {
	if raw, ok := w.(interface{ RawWriter() io.Writer }); ok {
		return raw.RawWriter()
	}
	return w
}

func funSpawn(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	argv := make([]string, len(list))
	out := w.Errout()
	if len(list) > 0 {
		i := 0
		for {
			argv[i] = list[i].String()
			io.WriteString(out, argv[i])
			i++
			if i >= len(list) {
				break
			}
			out.Write([]byte{' '})
		}
	}
	fmt.Fprintln(out)

	if f, ok := out.(interface{ Flush() error }); ok {
		f.Flush()
	}

	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = getRawWriter(w.Stdout())
	cmd.Stderr = getRawWriter(w.Errout())
	return gm.Null, cmd.Run()
}

func funExitCode(ctx context.Context, w *gm.World, arg gm.Node) (gm.Node, error) {
	e, err := gm.ExpectClass[gm.ErrorNode](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	var exitErr *exec.ExitError
	if !errors.As(e.Value, &exitErr) {
		return gm.Null, nil
	}
	return gm.Integer(exitErr.ExitCode()), nil
}

func funExitErrorP(ctx context.Context, w *gm.World, arg gm.Node) (gm.Node, error) {
	e, ok := arg.(gm.ErrorNode)
	if !ok {
		return gm.Null, nil
	}
	var exitErr *exec.ExitError
	if !errors.As(e.Value, &exitErr) {
		return gm.Null, nil
	}
	return gm.True, nil
}

func funExecutableNotFoundP(ctx context.Context, w *gm.World, arg gm.Node) (gm.Node, error) {
	e, ok := arg.(gm.ErrorNode)
	if !ok {
		return gm.Null, nil
	}
	if !errors.Is(e.Value, exec.ErrNotFound) {
		return gm.Null, nil
	}
	return gm.True, nil
}

func funSh(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	if w, ok := w.Stdout().(interface{ Flush() error }); ok {
		w.Flush()
	}
	if w, ok := w.Errout().(interface{ Flush() error }); ok {
		w.Flush()
	}
	for _, node := range list {
		s, err := gm.ExpectClass[gm.String](ctx, w, node)
		if err != nil {
			return nil, err
		}
		cmdline := s.String()
		cmd := newShell(cmdline)
		fmt.Fprintln(w.Errout(), cmdline)
		cmd.Stdout = getRawWriter(w.Stdout())
		cmd.Stderr = getRawWriter(w.Errout())
		if err := cmd.Run(); err != nil {
			return gm.Null, err
		}
	}
	return gm.Null, nil
}

func funCopy(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	return copyOrMove(ctx, w, list, "cp", func(s, d string) error {
		return file.Copy(s, d, false)
	})
}

func funMove(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	return copyOrMove(ctx, w, list, "mv", func(s, d string) error {
		return file.Move(s, d)
	})
}

func copyOrMove(ctx context.Context, w *gm.World, list []gm.Node, msg string, f func(s, d string) error) (gm.Node, error) {
	if len(list) < 2 {
		return nil, gm.ErrTooFewArguments
	}
	_destinate, err := gm.ExpectClass[gm.String](ctx, w, list[len(list)-1])
	if err != nil {
		return nil, err
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
		_source, err := gm.ExpectClass[gm.String](ctx, w, s)
		if err != nil {
			return nil, err
		}
		source := _source.String()

		var newFile string
		if isDir {
			newFile = filepath.Join(destinate, filepath.Base(source))
		} else {
			newFile = destinate
		}
		fmt.Printf("%s \"%s\" \"%s\"\n", msg, source, newFile)
		err = f(source, newFile)
		if err != nil {
			return gm.Null, err
		}
	}
	return gm.Null, nil
}

func funLookPath(ctx context.Context, w *gm.World, arg gm.Node) (gm.Node, error) {
	fname, err := gm.ExpectClass[gm.String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	result, err := exec.LookPath(string(fname))
	if err != nil {
		return gm.Null, nil
	}
	return gm.String(result), err
}
