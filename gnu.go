package main

import (
	"context"
	"path/filepath"
	"strings"

	gm "github.com/hymkor/gmnlisp"
)

func funBasename(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	_path, ok := args[0].(gm.String)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	path := _path.String()
	return gm.String(path[:len(path)-len(filepath.Ext(path))]), nil
}

func funDir(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	_path, ok := args[0].(gm.String)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	path := _path.String()
	return gm.String(filepath.Dir(path)), nil
}

func funNotDir(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	path1, ok := args[0].(gm.String)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	return gm.String(filepath.Base(path1.String())), nil
}

func funAbsPath(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	path1, ok := args[0].(gm.String)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	absPath, err := filepath.Abs(path1.String())
	if err != nil {
		return nil, err
	}
	return gm.String(absPath), nil
}

func funWildcard(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	var result gm.Node = gm.Null
	for i := len(list) - 1; i >= 0; i-- {
		pattern, ok := list[i].(gm.String)
		if !ok {
			return nil, gm.ErrExpectedString
		}
		match, err := filepath.Glob(pattern.String())
		if err != nil {
			return nil, err
		}
		for j := len(match) - 1; j >= 0; j-- {
			result = &gm.Cons{
				Car: gm.String(match[j]),
				Cdr: result,
			}
		}
	}
	return result, nil
}

func funShell(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	s, ok := list[0].(gm.String)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	cmdline := s.String()
	cmd := newShell(cmdline)
	cmd.Stdout = nil
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}
	o := string(output)
	o = strings.ReplaceAll(o, "\r", "") // CRLF -> LF
	o = strings.TrimSpace(o)
	return gm.String(o), nil
}
