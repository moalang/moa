package main

import (
	"crypto/tls"
	"fmt"
	"log"
	"net/http"
	"os"
)

func main() {
	errChan := make(chan error, 2)
	go func() { errChan <- launchHttpServer() }()
	go func() { errChan <- launchHttpsServer() }()
	err := <-errChan
	if err != nil {
		log.Fatalf("server failed to start: %v", err)
	}
	os.Exit(1)
}

func launchHttpServer() error {
	server := &http.Server{
		Addr: ":80",
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Location", "https://"+r.Host+r.URL.RequestURI())
			w.WriteHeader(301)
		})}
	return server.ListenAndServe()
}

type Service struct {
	Domain string
	Script string
}

func launchHttpsServer() error {
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
	services := make(map[string]*Service)
	services["localhost"] = &Service{
		Domain: "localhost",
		Script: "hello script",
	}
	server := &http.Server{
		Addr:      ":443",
		TLSConfig: tlsConfig,
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if service, ok := services[r.Host]; ok {
				path := fmt.Sprintf("static/%s/%s", r.Host, r.URL.Path)
				if r.URL.Path == "/" { // TODO: check the result of script evaluation
					http.ServeFile(w, r, path)
				} else {
					fmt.Fprintf(w, "%s", service.Script)
				}
			} else {
				w.WriteHeader(404)
				fmt.Fprintf(w, "%v", r.Host)
			}
		})}
	return server.ListenAndServeTLS("", "")
}
