package main

import (
	"testing"
)

func TestRuntime(t *testing.T) {
	if len(io_args()) != 4 {
		t.Fatal("io_args")
	}
	if string_size("hi") != 2 {
		t.Fatal("string_size")
	}

	if len(array(1, 2)) != 2 {
		t.Fatal("array")
	}
	if array_size([]int{1}) != 1 {
		t.Fatal("array_size")
	}
	if array_at([]int{1}, 0) != 1 {
		t.Fatal("array_at")
	}
	if len(array_slice([]int{1, 2, 3}, 1)) != 2 {
		t.Fatal("array_slice 1")
	}
	if len(array_slice([]int{1, 2, 3}, 1, 2)) != 1 {
		t.Fatal("array_slice 1 2")
	}
	if len(array_slice([]int{1, 2, 3}, 0, -2)) != 1 {
		t.Fatal("array_slice 0 (-2)")
	}
	if array_join([]int{1, 2}, ",") != "1,2" {
		t.Fatal("array_join")
	}

	if to_string("s") != "s" {
		t.Fatal("to_string")
	}
	if to_string(true) != "true" {
		t.Fatal("to_string")
	}
	if to_string(123) != "123" {
		t.Fatal("to_string")
	}
	if to_string(float32(1.3)) != "1.3" {
		t.Fatal("to_string")
	}
	if to_string(float64(1.3)) != "1.3" {
		t.Fatal("to_string")
	}
	if to_string([]any{1, 2}) != "[1 2]" {
		println(to_string([]any{1, 2}))
		t.Fatal("to_string")
	}
	io_puts("ok")
}
