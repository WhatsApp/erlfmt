#!/usr/bin/env bash

# Copyright (c) Meta Platforms, Inc. and affiliates.
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

# Requires bash version 5
# $ brew install bash

declare -A repos
declare -A comments

repos["OTP"]="../../../otp"
repos["kazoo"]="../../../kazoo"
repos["Inaka"]="../../../../inaka"
repos["MongooseIM"]="../../../MongooseIM"
repos["ejabberd"]="../../../ejabberd"
repos["WhatsApp"]="../../../../../../../local/whatsapp/server/erl"

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

function count() {
    find $1 -name '*.erl' -not -path '**SUITE_data/*' -not -name '*trpc_*' -not -name '*_pb.erl' \
    | xargs -I {} sh -c 'echo {}; awk "{print} /-module/ {exit}" {}' \
    | pcre2grep -Mo "^(?:%{$2}(?:\n|[^%\n][^\n]*\n))+" \
    | pcre2grep -Mv "%% @format\n\n" \
    | awk 'BEGIN {SUM = 0} !NF {SUM += 1} END { print SUM }'
}

echo "# Analysis: Number of percentages on module documentation"
echo "We have conducted a deeper analysis of the number of percentages used for module documentation. This is meant to guide our recommendation of the \`%%% % @format\` module pragma."
echo "From the empirical results below, it shows that while there is no explicit consensus, most projects prefer using \`%%%\` for module documentation, except for OTP which strongly favors \`%%\`."
echo "## Methodology"
echo "We first isolated the comments located above the \`-module\` attribute and counted the number of blocks (as opposed to single lines)"
echo "for each number of percentage, this avoids skewing the results towards lengthy multi-line comment blocks. We also removed all \`%% @format\` headings to give a better representation of usage."
echo "We have then outputted as a percentage of total blocks the number of percentages used by each repository."
echo "## Results"

for repo in ${!repos[*]}; do
    echo $repo
    for num_percentage in {1..3}; do
        comments[$num_percentage]=$(count ${repos[$repo]} $num_percentage)
    done

    repo_total=$(sum comments)

    for num_percentage in {1..3}; do
        printf "%s: %.1f%%\n" \
            `repeat $num_percentage "%"` \
            `eval "bc -l <<< \"${comments[$num_percentage]} * 100 / $repo_total\""`
    done

    printf '\n'
done

echo "Source: [./module_comments.sh](./module_comments.sh)"
