package main

import "fmt"

type IO struct{}

var io = new(IO)

func (_ IO) print(s string) {
	fmt.Printf(s)
}
