# tasty-coverage

A [tasty](https://hackage.haskell.org/package/tasty) ingredient which allows to generate per-test coverage data.

If the GHC compiler is passed the `-fhpc` flag, then the generated code is instrumented in order to also emit information about the executed parts of the sourcecode at runtime.
This coverage information is written to a file with the suffix `.tix`.
For testsuites it is sometimes more useful to have the coverage information for each test individually.
This information can be collected using the methods from the `Trace.Hpc.Reflect` module from the [hpc](https://hackage.haskell.org/package/hpc) library.
The `tasty-coverage` package provides a simple ingredient for the [tasty](https://hackage.haskell.org/package/tasty) testsuite driver which allows to run the testsuite with the `--report-coverage` option.
When this option is passed, one `.tix` file is generated for each individual test.
Passing tests have the file suffix `PASSED.tix`, whereas failing tests have the suffix `FAILED.tix`.

```console
> cabal run tasty-coverage-test -- --help
Mmm... tasty test suite

Usage: tasty-coverage-test [-p|--pattern PATTERN] [-t|--timeout DURATION] 
                           [--report-coverage]

Available options:
  -h,--help                Show this help text
  -p,--pattern PATTERN     Select only tests which satisfy a pattern or awk
                           expression
  -t,--timeout DURATION    Timeout for individual tests (suffixes: ms,s,m,h;
                           default: s)
  --report-coverage        Generate per-test coverage data

> cabal run tasty-coverage-test -- --report-coverage
Wrote coverage file: tix/UnitTests.testOne.PASSED.tix
Wrote coverage file: tix/UnitTests.testTwo.PASSED.tix
Wrote coverage file: tix/UnitTests.testThree.FAILED.tix
```
