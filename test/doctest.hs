-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc/lib", "src/cli/Main.hs"]

