package main

import (
	"bufio"
	"bytes"
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
		fmt.Print(runGoCommand(compileToGoCode(args), "build", "-ldflags=-s -w", "-trimpath", "-o", "a.out"))
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
	case "dev":
		fmt.Print(runGoCommand(compileToGoCode(args), "run"))
	case "test":
		fmt.Print(runGoCommand(compileToGoCode(args), "run"))
	case "go":
		fmt.Println(compileToGoCode(args))
	case "version":
		fmt.Println("moa v0.0.1 " + runtime.GOOS + "/" + runtime.GOARCH)
	default:
		fmt.Println(`Moa is a programming language

Usage: moa <command> [...arguments]

Commands:
  moa build [os] [arch]    compile to an executable file
  moa repl                 start a REPL session
  moa dev [...files]       execute as development mode
  moa test [...files]      run tests
  moa version              display Moa version`)
	}
}

func runGoCommand(gocode string, args ...string) string {
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
		fmt.Println(gocode)
		fmt.Println("--")
		fmt.Println(string(output))
		panic(err.Error())
	}
	return string(output)
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
	var suffix func(AST) AST
	var parse func() AST
	suffix = func(a AST) AST {
		if pos < len(tokens) {
			t := tokens[pos]
			if t.IsPrefixNotation() {
				pos++
				return makeAST(t, a, parse())
			}
		}
		return a
	}
	parse = func() AST {
		ast := makeAST(tokens[pos])
		pos++
		if ast.Token.Code == "def" {
			for pos < len(tokens) && tokens[pos].Lineno == ast.Token.Lineno {
				ast.Args = append(ast.Args, parse())
			}
		} else if ast.Token.IsId() {
			if pos < len(tokens) && tokens[pos].Code == "(" {
				pos++ // drop "("
				for pos < len(tokens) && tokens[pos].Code != ")" {
					ast.Args = append(ast.Args, parse())
				}
				pos++ // drop ")"
			}
		} else if ast.Token.IsLiteral() {
			// do nothing
		} else {
			fmt.Println(tokens)
			fmt.Println(asts)
			panic(fmt.Sprintf("Unknown %v", tokens[pos]))
		}
		return suffix(ast)
	}
	for pos < len(tokens) {
		asts = append(asts, parse())
	}
	return asts
}

func tokenize(code string, file string) []Token {
	tokens := []Token{Token{}}
	i := 0
	lineno := 1
	br := 0
	s1s := strings.Split("! ~ * / % + - | & ^ < > = ( ) [ ] { } .", " ")
	s2s := strings.Split("?? << >> || && == != <= >= += -= *= /= %= |= &= ^=", " ")
	s3s := strings.Split("<<= >>= ||= &&=", " ")
	push := func(code string) {
		tokens = append(tokens, Token{
			File:     file,
			Code:     code,
			Pos:      i,
			Lineno:   lineno,
			Column:   i - br + 1,
			PrevCode: tokens[len(tokens)-1].Code,
			PrevPos:  tokens[len(tokens)-1].Pos,
		})
	}
	for i = 0; i < len(code); i++ {
		if code[i] == '\n' {
			lineno += 1
			br = i
		} else if code[i] == ' ' {
			// ignore space
		} else if i+2 < len(code) && slices.Contains(s3s, code[i:i+3]) {
			push(code[i : i+3])
			i += 2
		} else if i+1 < len(code) && slices.Contains(s2s, code[i:i+2]) {
			push(code[i : i+2])
			i++
		} else if slices.Contains(s1s, code[i:i+1]) {
			push(code[i : i+1])
		} else if '0' <= code[i] && code[i] <= '9' {
			left := i
			for i < len(code) && (('0' <= code[i+1] && code[i+1] <= '9') || code[i+1] == '.') {
				i++
			}
			s := code[left : i+1]
			if strings.Contains(s, ".") {
				push(s)
			} else {
				push(s)
			}
		} else if ('A' <= code[i] && code[i] <= 'Z') || ('a' <= code[i] && code[i] <= 'z') || code[i] == '_' {
			left := i
			for i < len(code) && (('A' <= code[i+1] && code[i+1] <= 'Z') || ('a' <= code[i+1] && code[i+1] <= 'z') || code[i+1] == '_') {
				i += 1
			}
			push(code[left : i+1])
		} else if code[i] == '"' {
			left := i
			i++
			for i < len(code) && code[i] != '"' && code[i-1] != '\\' {
				i++
			}
			s := code[left : i+1]
			s = strings.Replace(s, "\\n", "\n", -1)
			s = strings.Replace(s, "\\t", "\t", -1)
			push(s)
		} else {
			panic("Unknown character `" + code[i:i+1] + "`")
		}
	}
	return tokens[1:]
}

func generateGoCode(asts []AST, isTest bool) string {
	codes := []string{`package main
import "fmt"
type IO struct {
  puts func(...any)
}
var io = IO {
  puts: func(a ...any) {
    fmt.Println(a...)
  },
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
	File     string
	Code     string
	Pos      int
	Lineno   int
	Column   int
	PrevCode string
	PrevPos  int
}

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

func (t Token) IsPrefixNotation() bool {
	return bytes.IndexByte([]byte("!~*/%+-|&^<>=.?"), t.Code[0]) >= 0
}

func (t Token) IsId() bool {
	return ('A' <= t.Code[0] && t.Code[0] <= 'Z') || ('a' <= t.Code[0] && t.Code[0] <= 'z') || t.Code[0] == '_'
}

func (t Token) IsLiteral() bool {
	return t.Code[0] == '"' || ('0' <= t.Code[0] && t.Code[0] <= '9')
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
	} else if a.Token.Code == "." {
		return a.Args[0].Gen() + "." + a.Args[1].Gen()
	} else if len(a.Args) == 0 {
		return a.Token.Code
	} else {
		if a.Token.IsPrefixNotation() {
			if len(a.Args) == 2 {
				return a.Args[0].Gen() + a.Token.Code + a.Args[1].Gen()
			} else if len(a.Args) == 1 {
				return a.Token.Code + a.Args[0].Gen()
			} else {
				panic("Unexpected length of operator")
			}
		} else {
			s := ""
			if a.Token.PrevCode != "." {
				s += "moa_"
			}
			s += a.Token.Code + "("
			for i, arg := range a.Args {
				if i > 0 {
					s += ","
				}
				s += arg.Gen()
			}
			return s + ")"
		}
	}
}

func (a AST) GoType() string {
	t := &a.Type
	for t != nil && t.Instance != nil {
		t = t.Instance
	}
	return t.TypeName
}
