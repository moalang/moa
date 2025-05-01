package main

import (
	"errors"
	"fmt"
)

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

func __dummy() {
	fmt.Print(errors.New("never print"))
}

func __use_error(_ error) {
}
