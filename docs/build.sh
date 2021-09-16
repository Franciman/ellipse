#!/bin/sh
pandoc -s --filter ./prooftreefilter.hs semantics.md -o semantics.html
