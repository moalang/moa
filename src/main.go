package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func main() {
	args := append(os.Args[1:], []string{""}...)
	switch args[0] {
	case "build":
		commandBuild()
	case "repl":
		commandREPL(args[1:])
	case "run":
		commandRun()
	case "test":
		commandTest()
	case "version":
		fmt.Println("moa v0.0.1 " + getPlatform())
	case "go-version":
		fmt.Println(strings.Trim(runGoCommand("version"), " "))
	default:
		fmt.Println(`Moa is a programming language

Usage: moa <command> [...arguments]

Commands:
  moa build [os] [arch]    compile to an executable file
  moa repl                 start a REPL session
  moa run                  execute the program
  moa test                 run tests
  moa version              display Moa version`)
	}
}

func commandBuild() {
	println("build not implemented yet")
}

func commandREPL(args []string) {
	if len(args) > 0 {
		code := strings.Join(args, " ")
		println("repl not implemented yet : " + code)
	} else {
		println("repl not implemented yet")
	}
}

func commandRun() {
	fmt.Println("hello")
}

func commandTest() {
	println("test not implemented yet")
}

func runGoCommand(args ...string) string {
	tempDir, err := os.MkdirTemp("", "moa-go-binary")
	if err != nil {
		panic(err.Error())
	}
	defer os.RemoveAll(tempDir)
	os.Setenv("GOROOT", tempDir)
	binaryPath := filepath.Join(tempDir, "go")
	err = os.WriteFile(binaryPath, getGoBinary(), 0755)
	if err != nil {
		panic(err.Error())
	}
	cmd := exec.Command(binaryPath, args...)
	output, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Println(string(output))
		panic(err.Error())
	}
	return string(output)
}
