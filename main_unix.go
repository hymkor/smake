//go:build !windows
// +build !windows

package main

import (
	"os"
	"os/exec"
)

func newShell(cmdline string) *exec.Cmd {
	cmd := exec.Command("sh", "-c", cmdline)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd
}
