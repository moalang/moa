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
	io_puts("ok")
}
