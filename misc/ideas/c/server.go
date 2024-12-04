package main

import (
    "fmt"
    "log"
    "net/http"
)

func main() {
    // ハンドラ関数を定義
    http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprint(w, "Hello")
    })

    // サーバ設定
    server := &http.Server{
        Addr:    ":8080",
        Handler: nil,
    }

    // サーバ起動
    log.Println("サーバをポート8080で起動します...")
    if err := server.ListenAndServe(); err != nil {
        log.Fatal(err)
    }
}

