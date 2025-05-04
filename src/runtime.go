package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
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

type __responseinit struct {
	status  int
	headers map[string][]string
	blob    []byte
	json    []byte
	html    string
	text    string
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
func __io_log[T any](x T) T {
	fmt.Fprintln(os.Stderr, x)
	return x
}
func __io_serve(listen string, f func(__request) __responseinit) {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		o := f(__request{})
		if o.status == 0 {
			o.status = 200
		}
		w.WriteHeader(o.status)
		h := w.Header()
		var err error
		if o.blob != nil {
			h.Set("content-type", "application/octet-stream")
			_, err = w.Write(o.blob)
		} else if o.json != nil {
			h.Set("content-type", "application/json; charset=utf-8")
			_, err = w.Write(o.json)
		} else if len(o.html) > 0 {
			h.Set("content-type", "text/html; charset=utf-8")
			_, err = w.Write([]byte(o.html))
		} else {
			h.Set("content-type", "text/plain; charset=utf-8")
			_, err = w.Write([]byte(o.text))
		}
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
		}
		for k, v := range o.headers {
			for _, s := range v {
				h.Add(k, s)
			}
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
