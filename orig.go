package main

import (
	"context"
	"fmt"
	"os"

	gm "github.com/hymkor/gmnlisp"
	"github.com/nyaosorg/go-windows-su"
)

func funShellExecute(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	var sup su.Param
	sup.Show = su.SHOWNORMAL

	switch len(list) {
	default:
		return nil, gm.ErrTooManyArguments
	case 4:
		dir, ok := list[3].(gm.String)
		if !ok {
			return nil, fmt.Errorf("[4]: %w", gm.ErrExpectedString)
		}
		sup.Directory = dir.String()
		fallthrough
	case 3:
		param, ok := list[2].(gm.String)
		if !ok {
			return nil, fmt.Errorf("[3]: %w", gm.ErrExpectedString)
		}
		sup.Param = param.String()
		fallthrough
	case 2:
		action, ok := list[0].(gm.String)
		if !ok {
			return nil, fmt.Errorf("[1]: %w", gm.ErrExpectedString)
		}
		sup.Action = action.String()

		path, ok := list[1].(gm.String)
		if !ok {
			return nil, fmt.Errorf("[2]: %w", gm.ErrExpectedString)
		}
		sup.Path = path.String()
	case 1:
		return nil, gm.ErrTooFewArguments
	case 0:
		return nil, gm.ErrTooFewArguments
	}
	rc, err := sup.ShellExecute()
	return gm.Integer(rc), err
}

func funIsDirectory(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	fnameStr, ok := args[0].(gm.String)
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
	if stat.IsDir() {
		return gm.True, nil
	}
	return gm.Null, nil
}
