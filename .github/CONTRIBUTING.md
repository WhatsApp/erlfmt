# Contributing to erlfmt
We want to make contributing to this project as easy and transparent as
possible.

## Pull Requests
We actively welcome your pull requests.

1. Fork the repo and create your branch from `master`.
2. Add tests for your code using [Common Test](https://erlang.org/doc/apps/common_test/basics_chapter.html).
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes using `rebar3 ct`.
5. Make sure your code passes dialyzer using `rebar3 dialyzer`.
6. If you haven't already, complete the Contributor License Agreement ("CLA").

## Contributor License Agreement ("CLA")
In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Issues
We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

### Changing layout rules
To propose a change to layout rules, please provide an example of original code,
current formatted output, proposed formatted output, and the new rule the
formatter should follow - please remember the rules should be as generic as possible.

### Security
Facebook has a [bounty program](https://www.facebook.com/whitehat/) for the safe
disclosure of security bugs. In those cases, please go through the process
outlined on that page and do not file a public issue.

## Coding Style
Format the project using itself!

## License
By contributing to erlfmt, you agree that your contributions will be licensed
under the LICENSE file in the root directory of this source tree.
