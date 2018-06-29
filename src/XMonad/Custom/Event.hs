-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Custom.Event
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-- Custom target for layouts, sublayouts and everything in between.
--
------------------------------------------------------------------------

module XMonad.Custom.Event
    ( handleEventHook'
    ) where

import           Data.Monoid
import           XMonad
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.Loggers.NamedScratchpad

handleEventHook' :: Event -> X All
handleEventHook' = mconcat [ nspTrackHook scratchpads
                           , docksEventHook
                           , fullscreenEventHook
                           ]
