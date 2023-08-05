# tasty-coverage

A [tasty](https://hackage.haskell.org/package/tasty) ingredient which allows to generate per-test coverage data.

```console
> cabal run tasty-test-test -- --help
Mmm... tasty test suite

Usage: tasty-test-test [-p|--pattern PATTERN] [-t|--timeout DURATION] 
                       [-c|--report-coverage]

Available options:
  -h,--help                Show this help text
  -p,--pattern PATTERN     Select only tests which satisfy a pattern or awk
                           expression
  -t,--timeout DURATION    Timeout for individual tests (suffixes: ms,s,m,h;
                           default: s)
  -c,--report-coverage     Generate per-test coverage data

> cabal run tasty-test-test -- -c
...
```
