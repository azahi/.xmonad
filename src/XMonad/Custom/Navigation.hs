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
    ( navigation2DConfig
    ) where

import           XMonad.Actions.Navigation2D

navigation2DConfig :: Navigation2DConfig
navigation2DConfig = def
    { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
    , floatNavigation        = hybridOf lineNavigation centerNavigation
    , layoutNavigation       = [("Full", centerNavigation)]
    , unmappedWindowRect     = [("Full", singleWindowRect)]
    }
