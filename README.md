# tasty-coverage

A [tasty](https://hackage.haskell.org/package/tasty) ingredient which allows to generate per-test coverage data.

The GHC compiler can be instructed with the `-fhpc` option to instrument the generated code in order to also emit information about parts of the sourcecode which are executed at runtime.
This coverage information is contained in a file with the suffix `.tix`, and contains the code which was covered during the entire runtime of the program.
For testsuites it is sometimes more useful to have the coverage information for each test individually.
This information can be collected using the methods from the `Trace.Hpc.Reflect` module from the [hpc](https://hackage.haskell.org/package/hpc) library.
This package provides a simple ingredient for the [tasty](https://hackage.haskell.org/package/tasty) testsuite driver.
This ingredient allows to run the testsuite with the `--report-coverage` option, and to generate one `.tix` file for each
individual test.
Passing tests have the file suffix `PASSED.tix`, whereas failing tests have the suffix `FAILED.tix`.

```console
> cabal run tasty-coverage-test -- --help
Mmm... tasty test suite

Usage: tasty-coverage-test [-p|--pattern PATTERN] [-t|--timeout DURATION] 
                           [-c|--report-coverage]

Available options:
  -h,--help                Show this help text
  -p,--pattern PATTERN     Select only tests which satisfy a pattern or awk
                           expression
  -t,--timeout DURATION    Timeout for individual tests (suffixes: ms,s,m,h;
                           default: s)
  -c,--report-coverage     Generate per-test coverage data

> cabal run tasty-coverage-test -- -c
Wrote coverage file: tix/UnitTests.testOne.PASSED.tix
Wrote coverage file: tix/UnitTests.testTwo.PASSED.tix
Wrote coverage file: tix/UnitTests.testThree.FAILED.tix
```
