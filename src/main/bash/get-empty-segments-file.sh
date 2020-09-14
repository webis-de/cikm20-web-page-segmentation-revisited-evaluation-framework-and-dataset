#!/bin/bash

if [ $# -ne 3 ];then
  echo "Usage: $0 <annotations.json> <fit-log> <output-directory>"
  exit 1
fi

annotations=$1
fit_log=$2
output_directory=$3
mkdir -p $output_directory

id=$(cat $annotations | grep -o '"id":"[0-9]*' | cut -d'"' -f4)
height=$(cat $annotations | grep -o '"height":[0-9]*' | cut -d':' -f2)
width=$(cat $annotations | grep -o '"width":[0-9]*' | cut -d':' -f2)

cat $fit_log \
  | awk 'BEGIN {
      number = 0
    } function writeFile() {
      if (segments != "") {
        printf "{\"id\":\"'$id'\",\"height\":'$height',\"width\":'$width',\"segmentations\":{\"%s\":[%s]}}\n", name, segments > "'"$output_directory"'/empty-segments-'$id'-"number".json"
      }

      name = $2
      number += 1
      segments = ""
    } $1 == "Fitting" {
      writeFile()
    } $1 == "removed-empty-segment:" {
      segment = $2
      if (segments != "") {
        segments = segments","
      }
      segments = segments""segment
    } END {
      writeFile()
    }'
