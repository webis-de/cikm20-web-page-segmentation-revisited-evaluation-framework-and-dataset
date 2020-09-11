#!/bin/bash

if [ $# -ne 1 ];then
  echo "Usage: $0 <fit-log>"
  exit 1
fi

input=$1

echo "#number segmentation area-precision area-recall area-f non-empty-ratio"
cat $input \
  | awk 'function printForSegmentation() {
        areaRecall = areaAB / areaA
        if (areaB == 0) {
          areaPrecision = 0
          areaF = 0
        } else {
          areaPrecision = areaAB / areaB
          areaF = 2 * areaPrecision * areaRecall / (areaPrecision + areaRecall)
        }
        nonEmptyRatio = (segments - emptySegments) / segments

        printf "%d %s %.2f %.2f %.2f %.2f\n", segmentation, segmentationName, areaPrecision, areaRecall, areaF, nonEmptyRatio

        areaA = 0
        areaB = 0
        areaAB = 0
        segments = 0
        emptySegments = 0
      } $1 ~ /Fitting/ {
        if (segments > 0) { printForSegmentation() }
        segmentationName = $2
        segmentation += 1
      } $1 ~ /original/ {
        areaA += $2
        areaB += $6
        areaAB += $8
        segments += 1
        if ($6 == 0) { emptySegments += 1 }
      } END {
        if (segments > 0) { printForSegmentation() }
      }'
