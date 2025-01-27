package main

import (
	"fmt"
	"os"
)

func string_size(s string) int {
	return len(s)
}

func array[T any](a ...T) []T {
	return a
}

func array_size[T any](a []T) int {
	return len(a)
}

func array_at[T any](a []T, n int) T {
	return a[n]
}

func array_slice[T any](a []T, n int, m ...int) []T {
	s := n
	e := len(a)
	if len(m) == 1 {
		e = m[0]
	}
	if s < 0 {
		s = len(a) - s - 1
	}
	if e < 0 {
		e = len(a) + e
	}
	return a[s:e]
}

func io_puts(a ...any) {
	fmt.Println(a...)
}

func io_args() []string {
	return os.Args
}
