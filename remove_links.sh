#!/bin/bash

# This takes in a links file and removes all of them.

homeDir=$HOME
links=`cut -d\| -f2 "$1" | sed "s#^~#$homeDir#g"`

for i in $links
do
    rm "$i"
done
