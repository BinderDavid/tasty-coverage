# Revision history for tasty-test

## Unreleased

* Be more specific with the file suffixes which are computed from the test outcome. Previously only "PASSED" and "FAILED" were used. We now also use "EXCEPTION", "TIMEOUT" and "SKIPPED".
* If the name of the test contains a path separator, then this path separator is removed from the name of the generated tix file (#5)

## 0.1.0.0 -- 2023-08-07

* Release of first version on Hackage
