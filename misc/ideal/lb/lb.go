package main

import (
	"crypto/tls"
	"fmt"
	"log"
	"net/http"
	"os"
)

func fileExists(filename string) bool {
	_, err := os.Stat(filename)
	if os.IsNotExist(err) {
		return false
	}
	return err == nil
}

func main() {
	certs := make(map[string]*tls.Certificate)
	tlsConfig := &tls.Config{
		GetCertificate: func(clientHello *tls.ClientHelloInfo) (*tls.Certificate, error) {
			name := clientHello.ServerName
			if cert, ok := certs[name]; ok {
				return cert, nil
			} else {
				cert, err := tls.LoadX509KeyPair(name+".pem", name+"-key.pem")
				if err != nil {
					return nil, err
				}
				certs[name] = &cert
				return &cert, err
			}
		},
	}

	server := &http.Server{
		Addr:      ":443",
		TLSConfig: tlsConfig,
	}
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello, you've hit %s\n", r.Host)
	})
	err := server.ListenAndServeTLS("", "")
	if err != nil {
		log.Fatalf("server failed to start: %v", err)
	}
}
