module Language.Module where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Map.Strict (Map)

import Data.List.NonEmpty (NonEmpty)

data Project decl
  = Project
      { prjModules :: IntMap (Module decl)
      , prjAdj :: IntMap [Int] -- we can build a graph from this, and then we can figure out build order
      }

data Module decl
  = Module
      { mPath :: FilePath
      , mName :: String
      , mImports :: [Import]
      , mSymbols :: Map String decl
      , mMisc :: [decl]
      }

data Import
  = Import (NonEmpty String) [String]