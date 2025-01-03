package main

import (
	"context"
	"path/filepath"
	"testing"

	gm "github.com/hymkor/gmnlisp"
)

func test(t *testing.T, code string, expect gm.Node) {
	ctx := context.Background()
	w := gm.New()
	v, f := setupFunctions(ctx, w, []string{})
	w = w.Let(v).Flet(f)
	if _, err := w.Interpret(ctx, embededLsp); err != nil {
		panic(err.Error())
	}
	if e := w.Assert(code, expect); e != "" {
		t.Helper()
		t.Fatal(e)
	}
}

func TestBasename(t *testing.T) {
	test(t, `(basename "hoge.tar")`, gm.String("hoge"))
}

func TestDir(t *testing.T) {
	test(t, `(dir "foo\\bar\\gar.tar")`, gm.String(`foo\bar`))
}

func TestNotDir(t *testing.T) {
	test(t, `(notdir "foo\\bar\\gar.tar")`, gm.String(`gar.tar`))
}

func TestAbs(t *testing.T) {
	temp, err := filepath.Abs(".")
	if err != nil {
		t.Fatal(err.Error())
	}
	test(t, `(abspath ".")`, gm.String(temp))
}
