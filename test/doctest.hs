{-# LANGUAGE OverloadedStrings #-}
import Test.DocTest
main :: IO ()
main = doctest ["-isrc", "src/Data/GELF.hs"]