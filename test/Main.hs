{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Hlog

prop_test :: Property
prop_test = property $ do
  doHlog === "Hlog"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
