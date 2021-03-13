{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Runtime.WAMMain where

import Data.Text
import NeatInterpolation (text)

wamMain :: Text -> Text -> Text
wamMain program query = [text|
  #include "wam.h"

  $program

  int main() {
    $query
  }
|]

