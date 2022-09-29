package main

import (
	"context"
	"path/filepath"
	"strings"

	gm "github.com/hymkor/gmnlisp"
)

func funNotDir(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	path1, ok := args[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	return gm.String(filepath.Base(path1.String())), nil
}

func funAbsPath(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	path1, ok := args[0].(gm.StringTypes)
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
	pattern, ok := list[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	match, err := filepath.Glob(pattern.String())
	if err != nil {
		return nil, err
	}
	var result gm.ListBuilder
	for _, s := range match {
		result.Add(gm.String(s))
	}
	return result.Sequence(), nil
}

func funShell(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	s, ok := list[0].(gm.StringTypes)
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
