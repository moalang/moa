package main

import "fmt"

type __Fn[T any] struct {
	fn   T
	code string
}

func (f __Fn[T]) String() string {
	return f.code
}

func __dummy() {
	fmt.Print("never print")
}
