#!/bin/bash
# This is currently necessary since we can't upload the testsuite...

sed -i '/executable tasty-coverage-example/,$d' tasty-coverage.cabal
cabal sdist
