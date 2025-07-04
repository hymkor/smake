package main

import (
	"context"
	_ "embed"
	"fmt"
	"os"
	"regexp"
	"strings"
	"time"

	gm "github.com/hymkor/gmnlisp"
	_ "github.com/hymkor/gmnlisp/eval"
)

const (
	stringTarget      = "$@"
	stringFirstSource = "$<"
	stringUpdated     = "$?"
)

var (
	rxEmbed = regexp.MustCompile(`\$(\(.*?\)|[\<\?\/\$\@])`)

	symbolTarget      = gm.NewSymbol(stringTarget)
	symbolFirstSource = gm.NewSymbol(stringFirstSource)
	symbolUpdated     = gm.NewSymbol(stringUpdated)
)

func dollar(ctx context.Context, w *gm.World) func(string) (string, bool, error) {
	assoc, err := w.Get(gm.NewSymbol("$"))
	if err != nil {
		return func(string) (string, bool, error) {
			return "", false, nil
		}
	}
	return func(s string) (string, bool, error) {
		pair, err := gm.Assoc(ctx, w, gm.String(s), assoc)
		if err != nil {
			return "", false, err
		}
		if gm.IsNull(pair) {
			return "", false, nil
		}
		cons, err := gm.ExpectClass[*gm.Cons](ctx, w, pair)
		if err != nil {
			return "", false, err
		}
		valueStr, err := gm.ExpectClass[gm.String](ctx, w, cons.Cdr)
		if err != nil {
			return "", false, err
		}
		return valueStr.String(), true, nil
	}
}

func joinSequence(ctx context.Context, w *gm.World, node gm.Node) string {
	var buffer strings.Builder
	if _, ok := node.(gm.Sequence); ok {
		gm.SeqEach(ctx, w, node, func(value gm.Node) error {
			if buffer.Len() > 0 {
				buffer.WriteByte(' ')
			}
			buffer.WriteString(value.String())
			return nil
		})
	} else {
		buffer.WriteString(node.String())
	}
	return buffer.String()
}

func expandLiteral(ctx context.Context, w *gm.World, s string) string {
	dic := dollar(ctx, w)
	return rxEmbed.ReplaceAllStringFunc(s, func(s string) string {
		if len(s) == 2 {
			switch s[1] {
			case '@':
				if val, err := w.Get(symbolTarget); err == nil {
					return val.String()
				}
			case '<':
				if val, err := w.Get(symbolFirstSource); err == nil {
					return val.String()
				}
			case '?':
				if list, err := w.Get(symbolUpdated); err == nil {
					return joinSequence(ctx, w, list)
				}
			case '/':
				return string(os.PathSeparator)
			case '$':
				return "$"
			}
			return s
		} else {
			key := s[2 : len(s)-1]
			//println("replace:", key)
			value, err := w.Get(gm.NewSymbol(key))
			if err != nil {
				if value, ok := os.LookupEnv(key); ok {
					return value
				}
				value, ok, err := dic(key)
				if err != nil {
					println(err.Error())
				} else if ok {
					return value
				}
			}
			return value.String()
		}
	})
}

func funExpandString(ctx context.Context, w *gm.World, arg gm.Node) (gm.Node, error) {
	s, err := gm.ExpectClass[gm.String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return gm.String(expandLiteral(ctx, w, s.String())), nil
}

func getStamp(ctx context.Context, w *gm.World, node gm.Node) (time.Time, error) {
	thePath, err := gm.ExpectClass[gm.String](ctx, w, node)
	if err != nil {
		return time.Time{}, err
	}
	theStat, err := os.Stat(thePath.String())
	if err != nil {
		if os.IsNotExist(err) {
			return time.Time{}, nil
		}
		return time.Time{}, err
	}
	return theStat.ModTime(), nil
}

func funUpdatep(ctx context.Context, w *gm.World, list []gm.Node) (gm.Node, error) {
	targetStamp, err := getStamp(ctx, w, list[0])
	if err != nil {
		return nil, err
	}
	var result gm.Node = gm.Null
	for i := 1; i < len(list); i++ {
		sourceStamp, err := getStamp(ctx, w, list[i])
		if err != nil {
			return gm.Null, err
		}
		if sourceStamp.After(targetStamp) {
			result = &gm.Cons{Car: list[i], Cdr: result}
		}
	}
	return result, nil
}

func shouldUpdate(ctx context.Context, w *gm.World, _list gm.Node) (bool, gm.Node, error) {
	targetNode, list, err := gm.Shift(_list)
	if err != nil {
		return false, nil, fmt.Errorf("%w: %#v", err, _list)
	}
	targetPath, err := gm.ExpectClass[gm.String](ctx, w, targetNode)
	if err != nil {
		return false, nil, err
	}
	targetInfo, err := os.Stat(targetPath.String())
	if err != nil {
		if os.IsNotExist(err) {
			return true, list, nil
		}
		return false, nil, fmt.Errorf("%w: '%s'", err, targetPath.String())
	}
	targetStamp := targetInfo.ModTime()

	var updatedFiles gm.Node = gm.Null
	for gm.HasValue(list) {
		var sourceNode gm.Node

		sourceNode, list, err = gm.Shift(list)
		if err != nil {
			return false, nil, fmt.Errorf("%w: ..%#v", err, list)
		}
		sourcePath, err := gm.ExpectClass[gm.String](ctx, w, sourceNode)
		if err != nil {
			return false, nil, err
		}
		sourceInfo, err := os.Stat(sourcePath.String())
		if err != nil {
			continue
		}
		sourceStamp := sourceInfo.ModTime()
		if sourceStamp.After(targetStamp) {
			updatedFiles = &gm.Cons{
				Car: sourceNode,
				Cdr: updatedFiles,
			}
		}
	}
	return gm.HasValue(updatedFiles), updatedFiles, nil
}

func doMake(ctx context.Context, w *gm.World, depend map[gm.String][2]gm.Node, rule [2]gm.Node) (bool, error) {
	// skip first (=target)
	_, sources, err := gm.Shift(rule[0])
	if err != nil {
		return false, err
	}
	for gm.HasValue(sources) {
		var source gm.Node
		var err error

		source, sources, err = gm.Shift(sources)
		if err != nil {
			return false, fmt.Errorf("%w: %#v", err, sources)
		}
		sourceStr, err := gm.ExpectClass[gm.String](ctx, w, source)
		if err != nil {
			return false, err
		}
		if _rule, ok := depend[sourceStr]; ok {
			if _, err := doMake(ctx, w, depend, _rule); err != nil {
				return false, err
			}
		}
	}
	isUpdate, updatedFiles, err := shouldUpdate(ctx, w, rule[0])
	if err != nil {
		return false, err
	}
	if !isUpdate {
		return false, nil
	}
	var target gm.Node = gm.String("")
	var firstSource gm.Node = gm.String("")
	if cons1, ok := rule[0].(*gm.Cons); ok {
		target = cons1.Car
		if cons2, ok := cons1.Cdr.(*gm.Cons); ok {
			firstSource = cons2.Car
		}
	}
	newWorld := w.Let(
		gm.Variables{
			symbolTarget:      target,
			symbolFirstSource: firstSource,
			symbolUpdated:     updatedFiles,
		})
	_, err = gm.Progn(ctx, newWorld, rule[1])
	if err != nil {
		return false, err
	}
	return true, nil
}

func cmdMake(ctx context.Context, w *gm.World, node gm.Node) (gm.Node, error) {
	var defaultTarget gm.String

	_defaultTarget, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	if val, ok := _defaultTarget.(gm.String); ok {
		defaultTarget = val
	}

	depend := map[gm.String][2]gm.Node{}

	for gm.HasValue(node) {
		var condAndAction gm.Node
		var err error

		condAndAction, node, err = gm.Shift(node)
		if err != nil {
			return nil, err
		}
		cond, action, err := w.ShiftAndEvalCar(ctx, condAndAction)
		if err != nil {
			return nil, fmt.Errorf("%w: %#v", err, condAndAction)
		}
		targetNode, _, err := gm.Shift(cond)
		if err != nil {
			return nil, fmt.Errorf("%w: %#v", err, condAndAction)
		}
		target, err := gm.ExpectClass[gm.String](ctx, w, targetNode)
		if err != nil {
			return nil, err
		}
		if defaultTarget == "" {
			defaultTarget = target
		}
		depend[target] = [...]gm.Node{cond, action}
	}
	startRule, ok := depend[defaultTarget]
	if !ok {
		return nil, fmt.Errorf("*** No rule to make target '%s'.  Stop.", defaultTarget)
	}
	isUpdate, err := doMake(ctx, w, depend, startRule)
	if err != nil {
		return gm.Null, err
	}
	if !isUpdate {
		fmt.Fprintf(os.Stderr, "'%s' is up to date.\n", defaultTarget)
	}
	return gm.Null, nil
}

func funFields(ctx context.Context, w *gm.World, arg gm.Node) (gm.Node, error) {
	s, err := gm.ExpectClass[gm.String](ctx, w, arg)
	if err != nil {
		return gm.Null, err
	}
	fields := strings.Fields(string(s))
	var result gm.Node = gm.Null
	for i := len(fields) - 1; i >= 0; i-- {
		result = &gm.Cons{
			Car: gm.String(fields[i]),
			Cdr: result,
		}
	}
	return result, nil
}
