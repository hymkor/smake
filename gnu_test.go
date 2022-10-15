package main

import (
	"path/filepath"
	"testing"

	gm "github.com/hymkor/gmnlisp"
)

func test(code string, expect gm.Node) string {
	w := gm.New().Let(setupFunctions([]string{}))
	return w.Assert(code, expect)
}

func TestBasename(t *testing.T) {
	if e := test(`(basename "hoge.tar")`, gm.String("hoge")); e != "" {
		t.Fatal(e)
	}
}

func TestDir(t *testing.T) {
	if e := test(`(dir "foo\\bar\\gar.tar")`, gm.String(`foo\bar`)); e != "" {
		t.Fatal(e)
	}
}

func TestNotDir(t *testing.T) {
	if e := test(`(notdir "foo\\bar\\gar.tar")`, gm.String(`gar.tar`)); e != "" {
		t.Fatal(e)
	}
}

func TestAbs(t *testing.T) {
	temp, err := filepath.Abs(".")
	if err != nil {
		t.Fatal(err.Error())
	}
	if e := test(`(abspath ".")`, gm.String(temp)); e != "" {
		t.Fatal(e)
	}
}
