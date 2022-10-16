package main

import (
	"os"
	"testing"

	gm "github.com/hymkor/gmnlisp"
)

func TestEnv(t *testing.T) {
	path := os.Getenv("PATH")

	// not existing ENV
	if e := test(`(env (("NOTEXIST" "YYYY")) (getenv "NOTEXIST"))`, gm.String("YYYY")); e != "" {
		t.Fatal(e)
	}
	if _, ok := os.LookupEnv("NOTEXIST"); ok {
		t.Fatal("(env) did not remove not exist variable")
	}

	// existing ENV
	if e := test(`(env (("PATH" "YYYY")) (getenv "PATH"))`, gm.String("YYYY")); e != "" {
		t.Fatal(e)
	}

	if path1 := os.Getenv("PATH"); path1 != path {
		t.Fatalf("PATH is not recovered as `%s` (%s)", path, path1)
	}
}
