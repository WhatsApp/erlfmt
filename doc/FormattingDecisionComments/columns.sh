#!/usr/bin/env bash

# Copyright (c) Meta Platforms, Inc. and its affiliates.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

for i in {0..100}
do
    printf "$i, "
    pcregrep -r --exclude-dir=".*SUITE_data" --exclude=".*trpc_.*" --exclude=".*_pb.erl" --include=".*\.erl" --include=".*\.hrl" "^[^\%\t]{$i}\%[^\%]" $1 | pcregrep -v ":(\s*)\%" | pcregrep -v "\S\s\%" | pcregrep -v "\S\%" | wc -l
done
