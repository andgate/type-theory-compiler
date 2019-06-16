module Main where

import Eval
import Check

main :: IO ()
main = do
  let r = eval undefined
  putStrLn "Hello, Haskell!"
