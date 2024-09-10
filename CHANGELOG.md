# Revision history for tasty-test

## 0.1.4.0 -- 2024-09-10

* Bump dependencies to be compatible with GHC 9.10
* Warn if the examined tix files are empty. This is usually an indication that the user forgot to enable the -fhpc flag. [#17](https://github.com/BinderDavid/tasty-coverage/pull/17)


## 0.1.3.0 -- 2023-09-15

* If multiple tests with exactly the same name occur in the testsuite, then we generate new filenames by appending tick-marks to the later tests until the name is unique.

## 0.1.2.0 -- 2023-09-10

* Add compatibility with Tasty 1.5
* Add `--tix-dir` flag for specifying directory for tix files.

## 0.1.1.0 -- 2023-09-07

* Be more specific with the file suffixes which are computed from the test outcome. Previously only "PASSED" and "FAILED" were used. We now also use "EXCEPTION", "TIMEOUT" and "SKIPPED".
* If the name of the test contains a path separator, then this path separator is removed from the name of the generated tix file (#5)
* Add commandline option "--remove-tix-hash" which replaces the hash in the tix files with "0". This is useful for golden testing the output of the tasty-coverage ingredient.

## 0.1.0.0 -- 2023-08-21

* Release of first version on Hackage
