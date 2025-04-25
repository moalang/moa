package main

import "fmt"

type __tuple1[A any] struct {
	v0 A
}

type __tuple2[A any, B any] struct {
	v0 A
	v1 B
}

func __dummy() {
	fmt.Print("never print")
}
