package main

import (
	"testing"

	gm "github.com/hymkor/gmnlisp"
)

func TestFunIsDirectory(t *testing.T) {
	test(t, `(-d ".")`, gm.True)
	test(t, `(-d "orig_test.go")`, gm.Null)
	test(t, `(-e "notfound.lsp")`, gm.Null)
	test(t, `(-e ".")`, gm.True)
	test(t, `(-e "notfound.lsp")`, gm.Null)
}
