package main

import (
	"context"
	"path/filepath"

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
