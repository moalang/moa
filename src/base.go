package main

import (
	"fmt"
	"io/ioutil"
)

type FS struct{}
type IO struct {
	fs FS
}
type Stream struct {
	utf8 string
}

var io = new(IO)

func (_ IO) print(s string) {
	fmt.Printf(s)
}

func (_ FS) open(path string) Stream {
	bytes, err := ioutil.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return Stream{utf8: string(bytes)}
}
