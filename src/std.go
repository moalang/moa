package main

import (
	"fmt"
)

type _IO struct {
	_put  func(...any)
	_puts func(...any)
}

var _io = _IO{
	_put: func(a ...any) {
		fmt.Print(a...)
	},
	_puts: func(a ...any) {
		fmt.Println(a...)
	},
}

func main() { _main() }
