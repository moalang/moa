package main

import (
	"bytes"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"strings"
	"time"
)

// ---( Embeded types )------------------------------------

type MoaError struct {
	Message  string
	Filename string
	Lineno   int
	Column   int
}

func (e MoaError) Error() string {
	return fmt.Sprintf("%s at %s:%d:%d", e.Message, e.Filename, e.Lineno, e.Column)
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

// ---( io misc )------------------------------------

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

// ---( io.serve )------------------------------------

type __io_request_struct struct {
	scheme   string
	method   string
	path     string
	version  string
	host     string
	ip       string
	_raw     *http.Request
	_queries map[string][]string
	_cookies map[string]string
}

type __io_request = *__io_request_struct

func __new_request(r *http.Request) __io_request {
	cookies := map[string]string{}
	for _, c := range r.Cookies() {
		cookies[c.Name] = c.Value
	}
	contentType := r.Header.Get("content-type")
	if contentType == "application/x-www-form-urlencoded" {
		err := r.ParseForm()
		if err != nil {
			panic(err)
		}
	} else if strings.HasPrefix(contentType, "multipart/form-data;") {
		err := r.ParseMultipartForm(128 * 1024)
		if err != nil {
			panic(err)
		}
	}
	ip, _, _ := net.SplitHostPort(r.RemoteAddr)
	return &__io_request_struct{
		r.URL.Scheme,
		r.Method,
		r.URL.Path,
		r.Proto,
		r.Host,
		ip,
		r,
		r.URL.Query(),
		cookies,
	}
}

func (r __io_request) header(key string) string {
	return r._raw.Header.Get(key)
}

func (r __io_request) cookie(key string) string {
	if v, ok := r._cookies[key]; ok {
		return v
	} else {
		return ""
	}
}

func (r __io_request) query(key string) string {
	if v, ok := r._queries[key]; ok {
		return v[0]
	} else {
		return ""
	}
}

func (r __io_request) form(key string) string {
	if v, ok := r._raw.PostForm[key]; ok {
		return v[0]
	} else {
		return ""
	}
}

func (r __io_request) file(key string) []byte {
	if fhs := r._raw.MultipartForm.File[key]; len(fhs) > 0 {
		file, err := fhs[0].Open()
		if err != nil {
			panic(err)
		}
		defer file.Close()
		b, err := io.ReadAll(file)
		if err != nil {
			panic(err)
		}
		return b
	}
	return []byte{}
}

func (r __io_request) filename(key string) string {
	if fhs := r._raw.MultipartForm.File[key]; len(fhs) > 0 {
		return fhs[0].Filename
	}
	return ""
}

func (r __io_request) blob() []byte {
	defer r._raw.Body.Close()
	b, err := io.ReadAll(r._raw.Body)
	if err != nil {
		panic(err)
	}
	return b
}

func (r __io_request) text() string {
	return string(r.blob())
}

type __io_response_init struct {
	status  int
	headers map[string][]string
	blob    []byte
	json    []byte
	html    string
	text    string
}

func __io_serve(listen string, f func(__io_request) __io_response_init) {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if r := recover(); r != nil {
				fmt.Fprintln(os.Stderr, r)
			}
		}()
		o := f(__new_request(r))
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
			panic(err)
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

// ---( io.fetch )------------------------------------

type __io_request_init struct {
	method  string
	headers map[string]string
	body    []byte
}

type __io_response_struct struct {
	_raw     *http.Response
	_cookies map[string]string
	status   int
}

type __io_response = *__io_response_struct

func (r __io_response) blob() []byte {
	defer r._raw.Body.Close()
	b, err := io.ReadAll(r._raw.Body)
	if err != nil {
		panic(err)
	}
	return b
}

func (r __io_response) text() string {
	return string(r.blob())
}

func (r __io_response) header(key string) string {
	return r._raw.Header.Get(key)
}

func (r __io_response) cookie(key string) string {
	if v, ok := r._cookies[key]; ok {
		return v
	}
	return ""
}

func __io_fetch(url string, ps ...__io_request_init) __io_response {
	p := __io_request_init{}
	if len(ps) >= 1 {
		p = ps[0]
	}
	if p.method == "" {
		p.method = "GET"
	}
	var body io.Reader
	if p.body != nil {
		body = bytes.NewBuffer(p.body)
	}
	req, err := http.NewRequest(p.method, url, body)
	if err != nil {
		panic(err)
	}
	if p.headers != nil {
		for k, v := range p.headers {
			req.Header.Set(k, v)
		}
	}

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}

	cookies := map[string]string{}
	for _, c := range resp.Cookies() {
		cookies[c.Name] = c.Value
	}

	return &__io_response_struct{
		resp,
		cookies,
		resp.StatusCode,
	}
}
