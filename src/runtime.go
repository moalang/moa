package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func string_size(s string) int64 {
	return int64(len(s))
}

func array[T any](a ...T) []T {
	return a
}

func array_size[T any](a []T) int64 {
	return int64(len(a))
}

func array_at[T any](a []T, n int64) T {
	return a[n]
}

func array_slice[T any](a []T, n int64, m ...int64) []T {
	ln := len(a)
	s := int(n)
	e := ln
	if len(m) == 1 {
		e = int(m[0])
	}
	if s < 0 {
		s = ln - s - 1
	}
	if e < 0 {
		e = ln + e
	}
	if s > ln {
		s = ln - 1
	}
	if e > ln {
		e = ln - 1
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
	case int32:
		return strconv.FormatInt(int64(v), 10)
	case int64:
		return strconv.FormatInt(v, 10)
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
