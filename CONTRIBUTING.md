Thanks for contributing to `tasty-coverage`. Issues, feature requests and pull requests are always welcome :)

# Code style

The code must be formatted using the `fourmolu` code formatter. See install instructions on their [website](https://github.com/fourmolu/fourmolu).
The following command should work:

```console
> fourmolu --mode inplace $(git ls-files '*.hs')
```

# Keeping a changelog

Any nontrivial change must be recorded in the CHANGELOG.md before the pull-request is merged.
We follow the [keepachangelog.com](https://keepachangelog.com/en/1.1.0/) standard.
Before a new version is released on Hackage, the changes are collected in the `Unreleased` section of the changelog.

# Releasing to Hackage

There is one difficulty which prevents us from releasing the package "as-is" to Hackage.
The testsuite requires the `tasty-coverage-example` to be compiled with the `-fhpc` option, since we test that during execution
the correct .tix files are generated. But hard-coding the `-fhpc` flag in the cabal file is an error according to `cabal check`
and Hackage won't let us upload the package. Therefore, the `create-sdist.sh` script removes the testsuite section from the cabal
file before `cabal sdist` is invoked.

# Release checklist

- Bump the version in the cabal file.
- Create a [git tag](https://git-scm.com/book/en/v2/Git-Basics-Tagging) for the new version and push it.
  Use `git tag -a x.x.x.x -m "Version x.x.x.x"` and then `git push origin x.x.x.x`
- Replace the `Unreleased` part of the `CHANGELOG.md` with the current date and the new version.
- Create a candidate on Hackage and check that everything is correct before releasing.

