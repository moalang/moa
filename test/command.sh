#!/bin/bash
. $(dirname $0)/common.inc
$MOA go-version | grep -q "go version go1.22.10 darwin/arm64"
$MOA version    | grep -q "moa v0.0.1 darwin/arm64"
