package main

import (
	"fmt"
	"net/http"
	"os"
)

func apiHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	fmt.Fprintf(w, "{\"message\": \"Hello world\"}")
}

func startHTTPServer(addr string, errChan chan error) {
	defer func() {
		if r := recover(); r != nil {
			errChan <- fmt.Errorf("HTTP server panic: %v", r)
		}
	}()
	fmt.Printf("listen HTTP %s", addr)
	err := http.ListenAndServe(addr, nil)
	if err != nil {
		errChan <- fmt.Errorf("HTTP server error: %v", err)
	}
}

func startHTTPSServer(addr string, errChan chan error, certFile string, keyFile string) {
	defer func() {
		if r := recover(); r != nil {
			errChan <- fmt.Errorf("HTTPS server panic: %v", r)
		}
	}()

	if _, err := os.Stat(certFile); os.IsNotExist(err) {
		errChan <- fmt.Errorf("HTTPS certificate file not found: %v", certFile)
		return
	}
	if _, err := os.Stat(keyFile); os.IsNotExist(err) {
		errChan <- fmt.Errorf("HTTPS key file not found: %v", keyFile)
		return
	}

	fmt.Printf("listen HTTPS %s", addr)
	err := http.ListenAndServeTLS(addr, certFile, keyFile, nil)
	if err != nil {
		errChan <- fmt.Errorf("HTTPS server error: %v", err)
	}
}

func isExist(path string) bool {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return false
	} else {
		return true
	}
}

func main() {
	httpAddr := ":8080"
	httpsAddr := ":8443"
	certFile := "tls.crt"
	keyFile := "tls.key"

	fs := http.FileServer(http.Dir("./static"))
	http.Handle("/", fs)
	http.HandleFunc("/api/", apiHandler)

	errChan := make(chan error, 2)
	go startHTTPServer(httpAddr, errChan)
	if isExist(certFile) && isExist(keyFile) {
		go startHTTPSServer(httpsAddr, errChan, certFile, keyFile)
	} else {
		println("not listen HTTPS because not found tls.crt or tls.key")
	}

	err := <-errChan
	fmt.Println("Error received, shutting down:", err)
	os.Exit(1)
}
