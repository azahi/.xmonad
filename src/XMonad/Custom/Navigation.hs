module XMonad.Custom.Navigation
    ( navigation2DConfig
    ) where

import XMonad.Actions.Navigation2D

navigation2DConfig :: Navigation2DConfig
navigation2DConfig = def
    { defaultTiledNavigation = hybridNavigation
    , floatNavigation        = hybridNavigation
    , layoutNavigation       = [("Full", centerNavigation)]
    , unmappedWindowRect     = [("Full", singleWindowRect)]
    }
