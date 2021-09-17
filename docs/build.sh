#!/bin/sh

filename="$1"
name=$(basename "$filename" .md)
output="${name}.html"
pandoc -s --filter ./prooftreefilter.hs --mathjax $filename -o $output
