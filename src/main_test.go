package main

import (
	"os"
	"testing"
)

func TestIO(t *testing.T) {
	if run("def main io.put(123)") != "123" {
		t.Errorf("failed")
	}
	if run("def main io.puts(123)") != "123\n" {
		t.Errorf("failed")
	}
}

func run(moa string) string {
	err := os.WriteFile("/tmp/test.moa", []byte(moa), 0644)
	if err != nil {
		panic(err)
	}
	return runGoCommand(compileToGoCode([]string{"dev", "/tmp/test.moa"}), "run")
}
