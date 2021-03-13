{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Runtime.WAMH where

import Data.Text as T
import NeatInterpolation (text)

wamH :: Text
wamH = [text| aaa |]
