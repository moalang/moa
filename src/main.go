package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
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
	return generateGoCode(parseMoaCodes(args[1:]), args[0] == "test")
}

func parseMoaCodes(args []string) []AST {
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
	asts := []AST{}
	for _, file := range targetFiles {
		buf, err := os.ReadFile(file)
		if err != nil {
			panic(err)
		}
		asts = append(asts, parseMoaCode(tokenize(string(buf), file))...)
	}
	return asts
}

func parseMoaCode(tokens []Token) []AST {
	makeAST := func(token Token, args ...AST) AST {
		return AST{Token: token, Args: args, Type: Type{TypeName: ""}}
	}
	pos := 0
	asts := []AST{}
	var parse func() AST
	parse = func() AST {
		ast := makeAST(tokens[pos])
		pos++
		if ast.Token.Code == "def" {
			for pos < len(tokens) && tokens[pos].Lineno == ast.Token.Lineno {
				ast.Args = append(ast.Args, parse())
			}
			return ast
		} else if ast.Token.Kind == KID {
			if pos < len(tokens) && tokens[pos].Code == "(" {
				pos++ // drop "("
				for pos < len(tokens) && tokens[pos].Code != ")" {
					ast.Args = append(ast.Args, parse())
				}
				pos++ // drop ")"
			}
			return ast
		} else if ast.Token.Kind == KINT || ast.Token.Kind == KFLOAT || ast.Token.Kind == KSTR {
			return ast
		} else {
			panic(fmt.Sprintf("Unknown %v", tokens[pos]))
		}
	}
	for pos < len(tokens) {
		asts = append(asts, parse())
	}
	return asts
}

func tokenize(code string, file string) []Token {
	tokens := []Token{}
	i := 0
	lineno := 1
	br := 0
	uop1s := strings.Split("! ~", " ")
	bop1s := strings.Split("* / % + - | & ^ < > =", " ")
	bop2s := strings.Split("?? << >> || && == != <= >=", " ")
	bop2as := strings.Split("+= -= *= /= %= |= &= ^=", " ")
	bop3as := strings.Split("<<= >>= ||= &&=", " ")
	symbols := strings.Split("( ) [ ] { } . =", " ")
	push := func(code string, kind int) {
		tokens = append(tokens, Token{
			File:   file,
			Code:   code,
			Lineno: lineno,
			Column: i - br + 1,
			Kind:   kind,
		})
	}
	for i = 0; i < len(code); i++ {
		if code[i] == '\n' {
			lineno += 1
			br = i
		} else if code[i] == ' ' {
			// ignore space
		} else if slices.Contains(symbols, code[i:i+1]) {
			push(code[i:i+1], KSYM)
		} else if slices.Contains(uop1s, code[i:i+1]) {
			push(code[i:i+1], KOP1)
		} else if slices.Contains(bop1s, code[i:i+1]) {
			push(code[i:i+1], KOP2)
		} else if slices.Contains(bop2s, code[i:i+2]) {
			push(code[i:i+2], KOP2)
			i++
		} else if slices.Contains(bop2as, code[i:i+2]) {
			push(code[i:i+2], KOP2A)
			i++
		} else if slices.Contains(bop3as, code[i:i+3]) {
			push(code[i:i+3], KOP2A)
			i += 2
		} else if '0' <= code[i] && code[i] <= '9' {
			left := i
			for i < len(code) && (('0' <= code[i+1] && code[i+1] <= '9') || code[i+1] == '.') {
				i++
			}
			s := code[left : i+1]
			if strings.Contains(s, ".") {
				push(s, KFLOAT)
			} else {
				push(s, KINT)
			}
		} else if ('A' <= code[i] && code[i] <= 'Z') || ('a' <= code[i] && code[i] <= 'z') || code[i] == '_' {
			left := i
			for i < len(code) && (('A' <= code[i+1] && code[i+1] <= 'Z') || ('a' <= code[i+1] && code[i+1] <= 'z') || code[i+1] == '_') {
				i += 1
			}
			push(code[left:i+1], KID)
		} else if code[i] == '"' {
			left := i
			i++
			for i < len(code) && code[i] != '"' && code[i-1] != '\\' {
				i++
			}
			s := code[left : i+1]
			s = strings.Replace(s, "\\n", "\n", -1)
			s = strings.Replace(s, "\\t", "\t", -1)
			push(s, KSTR)
		} else {
			panic("Unknown character `" + code[i:i+1] + "`")
		}
	}

	//fmt.Println(tokens)
	return tokens
}

func generateGoCode(asts []AST, isTest bool) string {
	codes := []string{`package main
import "fmt"
func moa_puts(a ...any) {
  fmt.Println(a...)
}`}
	if isTest {
		codes = append(codes, "\nfunc main() { fmt.Println(\"...............................ok\") }")
	} else {
		codes = append(codes, "\nfunc main() { moa_main() }")
		for _, ast := range asts {
			codes = append(codes, ast.Gen())
		}
	}
	return strings.Join(codes, "\n")
}

type Type struct {
	TypeName string
	Resolved bool
	Instance *Type
}

type Token struct {
	File   string
	Code   string
	Lineno int
	Column int
	Kind   int
}

const (
	KOP1 = iota
	KOP2
	KOP2A
	KSYM
	KINT
	KFLOAT
	KID
	KSTR
)

type AST struct {
	Token Token
	Args  []AST
	Type  Type
}

func (a AST) String() string {
	if len(a.Args) == 0 {
		return a.Token.Code
	} else {
		return fmt.Sprintf("%s%v", a.Token.Code, a.Args)
	}
}

func (t Token) String() string {
	return t.Code
}

func (a AST) Gen() string {
	if a.Token.Code == "def" {
		name := a.Args[0]
		args := []string{}
		for _, arg := range a.Args[0].Args {
			args = append(args, arg.Token.Code+" "+arg.GoType())
		}
		body := []string{}
		for _, arg := range a.Args[1:] {
			body = append(body, arg.Gen())
		}
		return fmt.Sprintf(`func moa_%s(%s) { %s }`, name, strings.Join(args, ", "), strings.Join(body, "\n"))
	} else if len(a.Args) == 0 {
		return a.Token.Code
	} else {
		s := "moa_" + a.Token.Code + "("
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
