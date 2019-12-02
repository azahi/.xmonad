-- |
-- Module      : XMonad.Custom.Prompt
-- Copyright   : (c) 2018-2019 Azat Bahawi <azahi@teknik.io>
-- License     : BSD3-style (see LICENSE)
-- Maintainer  : Azat Bahawi <azahi@teknik.io>
-- Stability   : unstable
-- Portability : unportable
--

module XMonad.Custom.Prompt
    ( listCompFunc
    , aListCompFunc
    , predicateFunction
    ) where

import           Data.Char
import           Data.List
import           XMonad.Prompt

listCompFunc :: XPConfig -> [String] -> String -> IO [String]
listCompFunc c xs s = return (filter (searchPredicate c s) xs)

aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs = listCompFunc c (map fst xs)

predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower
