package main

import (
	"os"
	"os/exec"
	"strings"
	"syscall"
)

func newShell(cmdline string) *exec.Cmd {
	var buffer strings.Builder

	buffer.WriteString(`/S /C "`)
	buffer.WriteString(cmdline)
	buffer.WriteString(` "`)

	cmdExe, ok := os.LookupEnv("COMSPEC")
	if !ok {
		cmdExe = "cmd.exe"
	}

	return &exec.Cmd{
		Path:        cmdExe,
		SysProcAttr: &syscall.SysProcAttr{CmdLine: buffer.String()},
		Stdin:       os.Stdin,
		Stdout:      os.Stdout,
		Stderr:      os.Stderr,
	}
}
