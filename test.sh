#! /usr/bin/env bash

cabal-dev install
dist/build/pasha/pasha someKey secret 0.01 10 10 program.pasha
