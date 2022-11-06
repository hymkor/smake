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

func trueOrNil(b bool) gm.Node {
	if b {
		return gm.True
	} else {
		return gm.Null
	}
}

func funStat(ctx context.Context, w *gm.World, args []gm.Node) (gm.Node, error) {
	_fname, ok := args[0].(gm.String)
	if !ok {
		return nil, gm.ErrExpectedString
	}
	fname := _fname.String()
	stat, err := os.Stat(fname)
	if err != nil {
		if os.IsNotExist(err) {
			return gm.Null, nil
		}
		return nil, err
	}
	var cons gm.Node = gm.Null

	cons = &gm.Cons{
		Car: &gm.Cons{Car: gm.NewSymbol("name"), Cdr: gm.String(stat.Name())},
		Cdr: cons,
	}
	cons = &gm.Cons{
		Car: &gm.Cons{Car: gm.NewSymbol("is-dir"), Cdr: trueOrNil(stat.IsDir())},
		Cdr: cons,
	}
	cons = &gm.Cons{
		Car: &gm.Cons{Car: gm.NewSymbol("size"), Cdr: gm.Integer(stat.Size())},
		Cdr: cons,
	}
	mt := stat.ModTime()
	cons = &gm.Cons{
		Car: gm.List(
			gm.NewSymbol("mod-time"),
			gm.Integer(mt.Year()),
			gm.Integer(mt.Month()),
			gm.Integer(mt.Day()),
			gm.Integer(mt.Hour()),
			gm.Integer(mt.Minute()),
			gm.Integer(mt.Second())),
		Cdr: cons,
	}
	cons = &gm.Cons{
		Car: &gm.Cons{
			Car: gm.NewSymbol("mod-time-unix"),
			Cdr: gm.Integer(mt.Unix()),
		},
		Cdr: cons,
	}
	return cons, nil
}
