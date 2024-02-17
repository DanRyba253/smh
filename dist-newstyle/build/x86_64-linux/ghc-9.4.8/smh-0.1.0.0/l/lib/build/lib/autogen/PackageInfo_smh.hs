{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_smh (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "smh"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "String manipulation tool written in haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
