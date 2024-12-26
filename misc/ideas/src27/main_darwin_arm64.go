package main

import _ "embed"

//go:embed go_binary/go_darwin_arm64
var goBinaryLinuxAmd64 []byte

func getGoBinary() []byte {
	return goBinaryLinuxAmd64
}

func getPlatform() string {
	return "darwin/arm64"
}
