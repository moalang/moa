package main

import (
	"errors"
	"fmt"
)

type MoaError struct {
	Message  string
	Filename string
	Lineno   int
	Column   int
}

func (e MoaError) Error() string {
	return fmt.Sprintf("%s at %s:%d:%d", e.Message, e.Filename, e.Lineno, e.Column)
}

type __io struct {
	put func(a ...any) int
}

var io = __io{
	put: func(a ...any) int { n, _ := fmt.Print(a...); return n },
}

type __tuple1[A any] struct {
	v0 A
}

type __tuple2[A any, B any] struct {
	v0 A
	v1 B
}

func __new_error(message string, filename string, lineno int, column int) error {
	return MoaError{message, filename, lineno, column}
}

func __catch[T any](v T, err error, f func(error) T) T {
	if err != nil {
		return f(err)
	} else {
		return v
	}
}

func __dummy() {
	fmt.Print(errors.New("never print"))
}
