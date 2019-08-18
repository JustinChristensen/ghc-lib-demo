#!/usr/bin/env bash

cd foo && ghc --make -v0 -ddump-if-trace -o foo Foo.hs
