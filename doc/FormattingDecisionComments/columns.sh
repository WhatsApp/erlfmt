#!/bin/bash

for i in {0..100}
do
    printf "$i, "
    pcregrep -r --exclude-dir=".*SUITE_data" --exclude=".*trpc_.*" --exclude=".*_pb.erl" --include=".*\.erl" --include=".*\.hrl" "^[^\%\t]{$i}\%[^\%]" $1 | pcregrep -v ":(\s*)\%" | pcregrep -v "\S\s\%" | pcregrep -v "\S\%" | wc -l
done