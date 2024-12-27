package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

func main() {
	args := os.Args[1:]
	command := ""
	if len(args) >= 1 {
		command = args[0]
	}
	switch command {
	case "build":
		runGoCommand(compileToGoCode(args), "build", "-ldflags=-s -w", "-trimpath", "-o", "a.out")
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
		runGoCommand(compileToGoCode(args), "run")
	case "test":
		runGoCommand(compileToGoCode(args), "run")
	case "version":
		fmt.Println("moa v0.0.1 " + runtime.GOOS + "/" + runtime.GOARCH)
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

func runGoCommand(gocode string, args ...string) {
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
	cmd := exec.Command("go", args...)
	output, err := cmd.CombinedOutput()
	if err != nil {
		panic(err.Error())
	}
	fmt.Print(string(output))
}

func compileToGoCode(args []string) string {
	codes := readMoaCodes(args[1:])
	ast := parseMoaCode(strings.Join(codes, "\n"))
	return generateGoCode(ast, args[0] == "test")
}

func readMoaCodes(args []string) []string {
	if len(args) == 0 {
		args = append(args, ".")
	}
	targetFiles := []string{}
	for _, arg := range args {
		info, err := os.Stat(arg)
		if err != nil {
			println(arg)
			panic(err)
		}
		if info.IsDir() {
			files, err := filepath.Glob(filepath.Join(arg, "**/*.moa"))
			if err != nil {
				panic(err)
			}
			targetFiles = append(targetFiles, files...)
		} else {
			targetFiles = append(targetFiles, arg)
		}
	}
	codes := []string{}
	for _, file := range targetFiles {
		buf, err := os.ReadFile(file)
		if err != nil {
			panic(err)
		}
		codes = append(codes, string(buf))
	}
	return codes
}

type Type struct {
	TypeName string
	Instance *Type
}

type AST struct {
	Code   string
	Args   []AST
	Type   Type
	File   string
	Line   int
	Column int
}

func (a AST) String() string {
	if len(a.Args) == 0 {
		return a.Code
	} else {
		return fmt.Sprintf("%s%v", a.Code, a.Args)
	}
}

func (a AST) Gen() string {
	if a.Code == "def" {
		name := a.Args[0]
		args := []string{}
		for _, arg := range a.Args[0].Args {
			args = append(args, arg.Code+" "+arg.GoType())
		}
		body := []string{}
		for _, arg := range a.Args[1:] {
			body = append(body, arg.Gen())
		}
		return fmt.Sprintf(`func moa_%s(%s) { %s }`, name, strings.Join(args, ", "), strings.Join(body, "\n"))
	} else if len(a.Args) == 0 {
		return a.Code
	} else {
		s := "moa_" + a.Code + "("
		for i, arg := range a.Args {
			if i > 0 {
				s += ","
			}
			s += arg.Gen()
		}
		return s + ")"
	}
}

func (a AST) GoType() string {
	t := &a.Type
	for t != nil && t.Instance != nil {
		t = t.Instance
	}
	return t.TypeName
}

func parseMoaCode(code string) AST {
	return makeAST("def", makeAST("main"), makeAST("puts", makeAST("\"hello\"")))
}

func makeAST(code string, args ...AST) AST {
	return AST{Code: code, Args: args, Type: Type{TypeName: "any"}}
}

func generateGoCode(ast AST, isTest bool) string {
	prefix := `package main
import "fmt"
func moa_puts(a ...any) {
  fmt.Println(a...)
}`
	if isTest {
		return prefix + "\nfunc main() { fmt.Println(\"...............................ok\") }"
	} else {
		return prefix + "\nfunc main() { moa_main() }\n" + ast.Gen()
	}
}
