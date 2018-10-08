-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Custom.Navigation
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Custom.Navigation
    ( navigation
    ) where

import           XMonad.Actions.Navigation2D

navigation :: Navigation2DConfig
navigation = def
    { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
    , floatNavigation        = hybridOf lineNavigation centerNavigation
    , layoutNavigation       = [("Full", centerNavigation)]
    , unmappedWindowRect     = [("Full", singleWindowRect)]
    }
