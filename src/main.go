package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"slices"
)

func main() {
	args := append(os.Args[1:], []string{""}...)
	switch args[0] {
	case "build":
		compile(generate(), "build", "-ldflags=-s -w", "-trimpath", "-o", "a.out")
	case "repl":
		reader := bufio.NewReader(os.Stdin)
		for {
			fmt.Print("> ")
			text, err := reader.ReadString('\n')
			if err != nil {
				panic(err)
			}
			fmt.Print(text)
		}
	case "run":
		fmt.Print(compile(generate(), "run"))
	case "test":
		fmt.Print(compile(generate("test"), "run"))
	case "version":
		fmt.Println("moa v0.0.1 " + runtime.GOOS + "/" + runtime.GOARCH)
	case "go-version":
		fmt.Print(runGoCommand("version"), " ")
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

func generate(options ...string) string {
	if slices.Contains(options, "test") {
		return `package main
import "fmt"
func main() { fmt.Println("...............................ok") }`
	} else {
		return `package main
import "fmt"
func main() { fmt.Println("hello") }`
	}
}

func compile(gocode string, args ...string) string {
	f, err := os.CreateTemp("", "main*.go")
	if err != nil {
		panic(err)
	}
	defer os.Remove(f.Name())
	_, err = f.Write([]byte(gocode))
	if err != nil {
		panic(err)
	}
	args = append(args, f.Name())
	return runGoCommand(args...)
}

func runGoCommand(args ...string) string {
	cmd := exec.Command("go", args...)
	output, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Println(string(output))
		panic(err.Error())
	}
	return string(output)
}
