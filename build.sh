#!/bin/sh

set -e

echo "DEBUGGING"

env

echo "PATH: $PATH"

sleep 2

exit 1

js="build/elm.js"
min="build/elm.min.js"

elm make --optimize --output=$js $@

uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$min

# echo "Initial size: $(cat $js | wc -c) bytes  ($js)"
# echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
# echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"

cp src/index.html build/
