package main

import (
	"context"
	"regexp"

	"github.com/hymkor/gmnlisp"
)

var rxCache = map[gmnlisp.String]*regexp.Regexp{}

func funMatch(ctx context.Context, w *gmnlisp.World, _pattern, _str gmnlisp.Node) (gmnlisp.Node, error) {

	pattern, err := gmnlisp.ExpectClass[gmnlisp.String](ctx, w, _pattern)
	if err != nil {
		return nil, err
	}
	str, err := gmnlisp.ExpectClass[gmnlisp.String](ctx, w, _str)
	if err != nil {
		return nil, err
	}

	re, ok := rxCache[pattern]
	if !ok {
		re, err = regexp.Compile(string(pattern))
		if err != nil {
			return nil, err
		}
	}
	matches := re.FindStringSubmatch(string(str))
	var result gmnlisp.Node = gmnlisp.Null
	for i := len(matches) - 1; i >= 0; i-- {
		result = &gmnlisp.Cons{
			Car: gmnlisp.String(matches[i]),
			Cdr: result,
		}
	}
	return result, nil
}
