# Release

The release process requires a few steps.

1. Update the [CHANGELOG.md](https://github.com/WhatsApp/erlfmt/blob/main/CHANGELOG.md) file.
```
    ### v<major>.<minor>.<bugfix>

    Document features and Bug fixes since last release.
    See https://github.com/WhatsApp/erlfmt/releases/ for link to commits since last release.
    Add issue numbers or pull requests numbers using hashtags to the fixes and features

    If only bug fixes change the bug fix version
    If features were added change the minor version
    Major version changes should be a big conversation
```
2. Bump version in `erlfmt.app.src`
3. Create a pull request, with these two changes and include that this is a release in the commit message.
4. Wait until github action passes and merge.
5. Draft a new release https://github.com/WhatsApp/erlfmt/releases/new
```
    Tag Version: v<major>.<minor>.<bugfix>
    Release title: Pick most significant feature
    Description: Copy ChangeLog contents
```
6. Release to hex: `rebar3 hex publish`.
7. Release to WhatsApp bumping thrird party code.


If you have not used hex before, create `~/.config/rebar3/rebar.config` and include the contents `{plugins, [rebar3_hex]}`:
```bash
$ mkdir -p ~/.config/rebar3/ && echo "{plugins, [rebar3_hex]}." >> ~/.config/rebar3/rebar.config
```

If you forgot your local password:
```bash
$ rebar3 hex user deauth
$ rebar3 hex user auth
```

Visit https://hex.pm/packages/erlfmt to see that it was published.
