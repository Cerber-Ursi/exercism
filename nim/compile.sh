#!/usr/bin/env bash

nim c $1.nim
mv ./$1 `dirname $0`/target/$1
`dirname $0`/target/$1