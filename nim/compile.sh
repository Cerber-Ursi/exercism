#!/usr/bin/env bash

nim c $1_test.nim
mv ./$1_test `dirname $0`/target/$1
`dirname $0`/target/$1