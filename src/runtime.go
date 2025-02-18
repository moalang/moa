package main

import (
	"encoding/binary"
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
	if _, err := fmt.Println(a...); err != nil {
		panic(err)
	}
}

func io_args() []string {
	return os.Args
}

func io_read(name string) []byte {
	b, err := os.ReadFile(name)
	if err != nil {
		panic(err)
	}
	return b
}

func io_write(name string, items ...any) {
	fh, err := os.OpenFile(name, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	if err != nil {
		panic(err)
	}
	for _, item := range items {
		if err = binary.Write(fh, binary.LittleEndian, item); err != nil {
			break
		}
	}
	if err1 := fh.Close(); err1 != nil && err == nil {
		err = err1
	}
	panic(err)
}

func to_string[T any](x T) string {
	switch v := any(x).(type) {
	case bool:
		return strconv.FormatBool(v)
	case float32:
		return strconv.FormatFloat(float64(v), 'f', -1, 32)
	case float64:
		return strconv.FormatFloat(v, 'f', -1, 64)
	case complex64:
		return strconv.FormatComplex(complex128(v), 'f', -1, 64)
	case complex128:
		return strconv.FormatComplex(v, 'f', -1, 128)
	case int:
		return strconv.FormatInt(int64(v), 10)
	case int8:
		return strconv.FormatInt(int64(v), 10)
	case int16:
		return strconv.FormatInt(int64(v), 10)
	case int32:
		return strconv.FormatInt(int64(v), 10)
	case int64:
		return strconv.FormatInt(v, 10)
	case uint:
		return strconv.FormatUint(uint64(v), 10)
	case uint8:
		return strconv.FormatUint(uint64(v), 10)
	case uint16:
		return strconv.FormatUint(uint64(v), 10)
	case uint32:
		return strconv.FormatUint(uint64(v), 10)
	case uint64:
		return strconv.FormatUint(v, 10)
	case uintptr:
		return strconv.FormatUint(uint64(v), 10)
	case string:
		return v
	case []byte:
		return string(v)
	case []any:
		return "[" + array_join(v, " ") + "]"
	default:
		panic(fmt.Sprintf("Unknown type: %v", any(x)))
	}
}
