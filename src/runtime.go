package main

import (
	"fmt"
	"reflect"
	"sort"
	"strconv"
	"strings"
)

type MoaVoid struct{}

type MoaOption[T any] struct {
	value T
	valid bool
}

type MoaTuple1[A any] struct {
	value1 A
}

type MoaTuple2[A any, B any] struct {
	value1 A
	value2 B
}

type MoaTuple3[A any, B any, C any] struct {
	value1 A
	value2 B
	value3 C
}

type MoaSet[T comparable] map[T]struct{}
type MoaMap[K comparable, V any] map[K]V

func (s MoaSet[T]) add(item T) {
	s[item] = struct{}{}
}

func (s MoaSet[T]) rid(item T) {
	delete(s, item)
}

func (s MoaSet[T]) has(item T) bool {
	_, ok := s[item]
	return ok
}

func moa__show(o any) string {
	switch v := any(o).(type) {
	case string:
		return strconv.Quote(v)
	case int:
		return strconv.Itoa(v)
	case int64:
		return strconv.FormatInt(v, 10)
	case float64:
		return strconv.FormatFloat(v, 'f', -1, 64)
	case bool:
		if v {
			return "true"
		}
		return "false"
	case rune:
		return string(v)
	case MoaVoid:
		return "moa-void"
	case fmt.Stringer:
		return v.String()
	case nil:
		return "<nil>"
	default:
		if reflect.TypeOf(v).Kind() == reflect.Slice {
			a := reflect.ValueOf(v)
			s := ""
			for i := 0; i < a.Len(); i++ {
				x := a.Index(i).Interface()
				s += moa__show(x) + " "
			}
			return "[" + s[:len(s)-1] + "]"
		}
		return fmt.Sprintf("<unsupported type> %v %v")
	}
}

func (m MoaSet[T]) String() string {
	s := []string{}
	for k, _ := range m {
		s = append(s, moa__show(k))
	}
	sort.Strings(s)
	return "set(" + strings.Join(s, " ") + ")"
}

func (m MoaMap[K, T]) String() string {
	s := []string{}
	for k, v := range m {
		s = append(s, moa__show(k)+" "+moa__show(v))
	}
	sort.Strings(s)
	return "map(" + strings.Join(s, " ") + ")"
}

func (t MoaOption[T]) String() string {
	if t.valid {
		return moa__show(t.value)
	}
	return "none"
}

func (t MoaTuple1[A]) String() string {
	return fmt.Sprintf("tuple(%s)", moa__show(t.value1))
}

func (t MoaTuple2[A, B]) String() string {
	return fmt.Sprintf("tuple(%s %s)", moa__show(t.value1), moa__show(t.value2))
}

func (t MoaTuple3[A, B, C]) String() string {
	return fmt.Sprintf("tuple(%s %s %s)", moa__show(t.value1), moa__show(t.value2), moa__show(t.value3))
}

var moa_void = MoaVoid{}

const moa_true = true
const moa_false = false

var moa_none = MoaOption[int]{valid: false}

func dummy() {
	fmt.Printf("")
}

func moa_vec[T any](args ...T) []T {
	return args
}

func moa_set[T comparable](args ...T) MoaSet[T] {
	s := MoaSet[T]{}
	for _, x := range args {
		s[x] = struct{}{}
	}
	return s
}

func moa_map2[K comparable, V any](k K, v V) MoaMap[K, V] {
	return map[K]V{k: v}
}

func moa_map4[K comparable, V any](k1 K, v1 V, k2 K, v2 V) MoaMap[K, V] {
	return map[K]V{k1: v1, k2: v2}
}

func moa_map6[K comparable, V any](k1 K, v1 V, k2 K, v2 V, k3 K, v3 V) MoaMap[K, V] {
	return map[K]V{k1: v1, k2: v2, k3: v3}
}

func moa_tuple1[A any](a A) MoaTuple1[A] {
	return MoaTuple1[A]{value1: a}
}

func moa_tuple2[A any, B any](a A, b B) MoaTuple2[A, B] {
	return MoaTuple2[A, B]{value1: a, value2: b}
}

func moa_tuple3[A any, B any, C any](a A, b B, c C) MoaTuple3[A, B, C] {
	return MoaTuple3[A, B, C]{value1: a, value2: b, value3: c}
}

func moa_some[T any](value T) MoaOption[T] {
	return MoaOption[T]{value: value, valid: true}
}
