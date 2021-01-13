#!/bin/bash

echo "file segmentation num-segments"
for input in $@;do
  cat $input \
    | jq '.segmentations | map_values(length)' \
    | grep ":" \
    | sed 's/^ *"//' \
    | sed 's/"://' \
    | sed 's/,$//' \
    | awk '{print "'"$input"' "$0}'
done
