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

import os

paths = [
    ("OTP", "../../../otp"),
    ("kazoo", "../../../kazoo"),
    ("Inaka", "../../../../inaka"),
    ("MongooseIM", "../../../MongooseIM"),
    ("ejabberd", "../../../ejabberd"),
    ("WhatsApp", "~/local/whatsapp/server/erl"),
]

pattern_types = {
    # standalone comment (not at column zero) with number of percentages
    "standalone": {
        "1": r'"^(\s)+\%[^\%]"',
        "2": r'"^(\s)+\%\%[^\%]"',
        "3": r'"^(\s)+\%\%\%[^\%]"',
    },
    # following comment with 0 or 1 space and number of percentages
    "following": {
        "1": r'"[^\s\%](\s)?\%[^\%]"',
        "2": r'"[^\s\%](\s)?\%\%[^\%]"',
        "3": r'"[^\s\%](\s)?\%\%\%[^\%]"',
    },
    # aligned comment 2 or more spaces and number of percentages
    "aligned": {
        "1": r'"[^\s\%](\s\s+)\%[^\%]"',
        "2": r'"[^\s\%](\s\s+)\%\%[^\%]"',
        "3": r'"[^\s\%](\s\s+)\%\%\%[^\%]"',
    },
}

def count(path, pattern):
    excludes = r'--exclude-dir=".*SUITE_data" --exclude=".*trpc_.*" --exclude=".*_pb.erl"'
    includes = r'--include=".*\.erl" --include=".*\.hrl"'
    cmd_string = f"pcregrep -r {excludes} {includes} {pattern} {path} | wc -l"
    out = os.popen(cmd_string).read()
    return int(out)

def percentage(count, total):
    return int((count * 100)/total)

def counts(path, patterns):
    return {
        key: count(path, patterns[key])
        for key in ["1", "2", "3"]
    }

count_results = {}
for name, path in paths:
    count_results[name] = {
        pattern_type: counts(path, patterns)
        for pattern_type, patterns in pattern_types.items()
    }

totals = {}
for name, counts_per in count_results.items():
    total = 0
    for pattern_type, counts_per_number in counts_per.items():
        for number, count in counts_per_number.items():
            total += count
    totals[name] = total

percentage_results = {}
for name, counts_per in count_results.items():
    percentage_results[name] = {}
    for pattern_type, counts_per_number in counts_per.items():
        the_percentages = {}
        for number, count in counts_per_number.items():
            the_percentages[number] = percentage(count, totals[name])
        percentage_results[name][pattern_type] = the_percentages

print("# Analysis: Usage of Types of Comments")
print()
print("We have analysed comments in several code bases, looking at the following two factors:")
print()
print("  - The number of percentages symbols")
print("  - Whether comments are standalone, following directly or aligned:")
print("    * standalone: a comment is on its own line, with possibly whitespace, but no code.")
print("    * following: a comment is on the same line as code, but only zero or one space is between it and the code.")
print("    * aligned: a comment is on the same line as code, more than one space away from the code.")
print()
for name, percentages_per in percentage_results.items():
    print(f"## {name}")
    for pattern_type, percentages_per_number in percentages_per.items():
        print(f"{pattern_type}:")
        for number, percentage in percentages_per_number.items():
            print(f" - {number}: {percentage}")
    print()
