#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
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

declare -A repos
declare -A comments

repos["OTP"]="../../../otp"
repos["kazoo"]="../../../kazoo"
repos["Inaka"]="../../../inaka"
repos["MongooseIM"]="../../../MongooseIM"
repos["ejabberd"]="../../../ejabberd"
repos["WhatsApp"]="../../../whatsapp/server/erl"

function sum() {
    local -n array=$1
    for i in ${array[@]}; do
        let sum+=$i
    done
    echo $sum
}

function repeat() {
    printf "%$1s" | tr " " $2
}

for repo in ${!repos[*]}; do
    echo $repo

    for num_percentage in {1..3}; do
        comments[$num_percentage]=`eval "find ${repos[$repo]} -name '*.erl' -not -path '**SUITE_data/*' -not -name '*trpc_*' -not -name '*_pb.erl' -print0 \
        | xargs -0 -I %1 -n1 sh -c 'echo %1; sed -n \"0,/-module/p\" %1 | pcre2grep -Mo \"^(?:%{$num_percentage}(?:\n|[^%\n][^\n]*\n))+\" \
        | pcre2grep -Mv \"%% @format\n\n\"' | awk 'BEGIN {SUM = 0} !NF {SUM += 1} END { print SUM }'"`
    done

    repo_total=$(sum comments)

    for num_percentage in {1..3}; do
        printf "%s: %.1f%%\n" \
            `repeat $num_percentage "%"` \
            `eval "bc -l <<< \"${comments[$num_percentage]} * 100 / $repo_total\""`
    done

    printf '\n'
done
