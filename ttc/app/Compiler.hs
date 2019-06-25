module Compiler where

data Compiler =
  Compiler
    { cInputs :: [String]
    , cOutput :: String
    , cOutputIR :: Bool
    }
    deriving (Show)

runCompiler :: Compiler -> IO ()
runCompiler c = do
  print c