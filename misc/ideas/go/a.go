package main

import (
	_ "embed"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
)

//go:embed go
var goBinary []byte

func main() {
	// 一時ディレクトリにバイナリを書き出す
	tmpDir, err := ioutil.TempDir("", "embed_go")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(tmpDir)

	goPath := filepath.Join(tmpDir, "go_binary")
	err = ioutil.WriteFile(goPath, goBinary, 0755)
	if err != nil {
		panic(err)
	}

	// 埋め込んだgoコマンドを実行する例
	cmd := exec.Command(goPath, "run", "hello.go")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err = cmd.Run()
	if err != nil {
		panic(err)
	}
}
