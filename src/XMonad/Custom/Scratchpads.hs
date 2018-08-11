-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Custom.Scratchpads
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Custom.Scratchpads
    ( scratchpads
    ) where

import           XMonad.Core
import qualified XMonad.Custom.Misc          as CM
import           XMonad.ManageHook
import qualified XMonad.StackSet             as S
import           XMonad.Util.NamedScratchpad

spawnTerminalWith :: String -> String -> String
spawnTerminalWith t c = CM.term CM.customApplications ++ " -title " ++ t ++ " -e " ++ c

floatingNSP :: ManageHook
floatingNSP = customFloating $ S.RationalRect x y w h
    where
        x = (1 - w) / 2
        y = (1 - h) / 2
        w = 1 / 2
        h = 1 / 2.5

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "console"
         (spawnTerminalWith "NSPConsole" "~/.xmonad/bin/nsp-console.sh")
         (title =? "NSPConsole")
         floatingNSP
    , NS "volume"
         (spawnTerminalWith "NSPVolume" (CM.mixer CM.customApplications))
         (title =? "NSPVolume")
         floatingNSP
    , NS "music"
         (spawnTerminalWith "NSPMusic" "~/.bin/mp")
         (title =? "NSPMusic")
         floatingNSP
    , NS "top"
         (spawnTerminalWith "NSPTop" (CM.top CM.customApplications))
         (title =? "NSPTop")
         floatingNSP
    ]
