package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
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

func array_join[T any](a []T, s string) string {
	xs := []string{}
	switch v := any(a).(type) {
	case []string:
		xs = v
	default:
		for _, element := range a {
			xs = append(xs, to_string(element))
		}
	}
	return strings.Join(xs, s)
}

func io_puts(a ...any) {
	fmt.Println(a...)
}

func io_args() []string {
	return os.Args
}

func to_string[T any](x T) string {
	switch v := any(x).(type) {
	case string:
		return v
	case bool:
		return strconv.FormatBool(v)
	case int:
		return strconv.Itoa(v)
	case float32:
		return strconv.FormatFloat(float64(v), 'f', -1, 32)
	case float64:
		return strconv.FormatFloat(v, 'f', -1, 64)
	case []any:
		return "[" + array_join(v, " ") + "]"
	default:
		panic(fmt.Sprintf("Unknown type: %v", any(x)))
	}
}
