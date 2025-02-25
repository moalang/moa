package main

import (
	"fmt"
	"io"
	"os"
	"os/exec"
)

func main() {
	args := os.Args
	if args[0] == "run" {
		data, err := io.ReadAll(os.Stdin)
		if err != nil {
			panic(err)
		}
		if err = os.WriteFile("/tmp/moa.go", []byte(compile(string(data))), 0644); err != nil {
			panic(err)
		}
		out, err := exec.Command("go", "run", "/tmp/moa.go").Output()
		if err != nil {
			panic(err)
		}
		fmt.Println(string(out))
	} else {
		println(`Moa is a modern programming language, many-core optimized, type-safe with optional typing, dev tools, and a test runner.

Usage:
  moa                                Launch interactive shell
  moa dev [<path>]                   Execute files as dev mode
  moa build [-o output] [<path>]     Compile files
  moa test [<path>]                  Run tests`)
	}
}
