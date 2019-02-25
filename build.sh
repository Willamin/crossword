#!/bin/sh

set -e

elm make --optimize --output="build/elm.js" "src/Main.elm"

cp "src/index.html" "build/"
