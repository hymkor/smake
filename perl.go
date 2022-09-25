package main

import (
	"context"
	"os"

	gm "github.com/hymkor/gmnlisp"
)

func fileTest(w *gm.World, args []gm.Node, test func(os.FileInfo) bool) (gm.Node, error) {
	fnameStr, ok := args[0].(gm.StringTypes)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	fname := fnameStr.String()
	stat, err := os.Stat(fname)
	if err != nil {
		if os.IsNotExist(err) {
			return gm.Null, nil
		}
		return nil, err
	}
	if test(stat) {
		return gm.True, nil
	}
	return gm.Null, nil
}

func funIsExist(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	return fileTest(w, args, func(f os.FileInfo) bool {
		return true
	})
}

func funIsDirectory(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	return fileTest(w, args, func(f os.FileInfo) bool {
		return f.IsDir()
	})
}
