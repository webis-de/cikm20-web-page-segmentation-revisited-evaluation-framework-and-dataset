#!/bin/bash

input=$1
sigma=$2
upper_percent=$3

radius=0
lower_percent=1

output=$(dirname $1)/screenshot-canny-"$radius"x"$sigma"-"$lower_percent"-"$upper_percent".png
if [ -e $output ];then
  echo "Skipping existing $output"
else
  echo "$input to $output"
  convert $input -canny $radius"x"$sigma+$lower_percent%+$upper_percent% $output
  if [ $? -ne 0 ];then
    echo "Failed"
    exit 1
  fi
fi
