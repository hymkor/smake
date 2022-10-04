package main

import (
	"context"
	"fmt"

	gm "github.com/hymkor/gmnlisp"
	"github.com/nyaosorg/go-windows-su"
)

func newSequenceBuilder(s gm.Node) gm.SeqBuilder {
	if _, ok := s.(gm.UTF8String); ok {
		return &gm.UTF8StringBuilder{}
	}
	if _, ok := s.(gm.UTF32String); ok {
		return &gm.UTF32StringBuilder{}
	}
	return &gm.ListBuilder{}
}

func funSplitSequence(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	sep := list[0]
	str := list[1]

	var result gm.ListBuilder
	subSeq := newSequenceBuilder(list[1])

	gm.SeqEach(str, func(value gm.Node) error {
		if value.Equals(sep, gm.EQUAL) {
			result.Add(subSeq.Sequence())
			subSeq = newSequenceBuilder(str)
		} else {
			subSeq.Add(value)
		}
		return nil
	})
	result.Add(subSeq.Sequence())
	return result.Sequence(), nil
}

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
