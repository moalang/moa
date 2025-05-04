package main

import (
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
)

type MoaError struct {
	Message  string
	Filename string
	Lineno   int
	Column   int
}

type __tuple1[A any] struct {
	v0 A
}

type __tuple2[A any, B any] struct {
	v0 A
	v1 B
}

type __tuple3[A any, B any, C any] struct {
	v0 A
	v1 B
	v2 C
}

type __request struct {
}

type __response struct {
	status  int
	headers map[string][]string
	body    io.ReadCloser
}

func (_ __request) respond(status int, headers map[string][]string, text string) __response {
	return __response{status, headers, io.NopCloser(strings.NewReader(text))}
}

func (r __response) text() string {
	b, err := io.ReadAll(r.body)
	if err != nil {
		panic(err)
	}
	err = r.body.Close()
	if err != nil {
		panic(err)
	}
	return string(b)
}

func (e MoaError) Error() string {
	return fmt.Sprintf("%s at %s:%d:%d", e.Message, e.Filename, e.Lineno, e.Column)
}

func __io_sleep(sec float64) {
	time.Sleep(time.Duration(sec * float64(time.Second)))
}
func __io_put(a ...any) int {
	n, err := fmt.Print(a...)
	if err != nil {
		panic(err)
	}
	return n
}
func __io_puts(a ...any) int {
	n, err := fmt.Println(a...)
	if err != nil {
		panic(err)
	}
	return n
}
func __io_serve(listen string, f func(__request) __response) {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		o := f(__request{})
		w.WriteHeader(o.status)
		h := w.Header()
		for k, v := range o.headers {
			for _, s := range v {
				h.Add(k, s)
			}
		}
		_, err := io.Copy(w, o.body)
		if err != nil {
			o.body.Close()
			__io_log(err)
		}
		err = o.body.Close()
		if err != nil {
			__io_log(err)
		}
	})
	err := http.ListenAndServe(listen, nil)
	if err != nil {
		panic(err)
	}
}
func __io_fetch(url string) __response {
	r, err := http.Get(url)
	if err != nil {
		panic(err)
	}
	return __response{
		r.StatusCode,
		r.Header,
		r.Body,
	}
}
