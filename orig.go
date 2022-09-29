package main

import (
	"context"

	gm "github.com/hymkor/gmnlisp"
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
