export LC_ALL=C
set -e -x
MOA=/tmp/moa_test/moa
t=/tmp/moa_test/$(basename "$0" .sh)
mkdir -p $t
echo -n "Testing $t ..."
