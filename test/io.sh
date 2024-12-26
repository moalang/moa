#!/bin/bash
. $(dirname $0)/common.inc
echo 'def main puts("hello")' > $t/hello.moa
(cd $t && $MOA run | grep -q hello)
