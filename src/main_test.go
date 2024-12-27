package main

import (
	"log"
	"os"
	"testing"
)

func TestStub(t *testing.T) {
	eq := func(expect, code string) {
		if run("def main io.puts("+code+")") != expect+"\n" {
			t.Errorf("failed")
		}
	}
	eq("true", "true")
	eq("hi", "\"hi\"")
	eq("123", "123")
	eq("3", "1+2")
}

func run(moa string) string {
	f, err := os.CreateTemp("", "test*.moa")
	if err != nil {
		panic(err)
	}
	defer os.Remove(f.Name())
	if _, err := f.Write([]byte(moa)); err != nil {
		log.Fatal(err)
	}
	if err := f.Close(); err != nil {
		log.Fatal(err)
	}
	args := []string{"run", f.Name()}
	return runGoCommand(compileToGoCode(args), "run")
}
