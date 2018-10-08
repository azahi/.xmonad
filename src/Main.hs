-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Main where

import           XMonad
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.Navigation2D
import qualified XMonad.Custom.Bindings         as C
import qualified XMonad.Custom.Event            as C
import qualified XMonad.Custom.Layout           as C
import qualified XMonad.Custom.Log              as C
import qualified XMonad.Custom.Manage           as C
import qualified XMonad.Custom.Misc             as C
import qualified XMonad.Custom.Navigation       as C
import qualified XMonad.Custom.Projects         as C
import qualified XMonad.Custom.Startup          as C
import qualified XMonad.Custom.Theme            as C
import qualified XMonad.Custom.Workspaces       as C
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import qualified XMonad.StackSet                as S
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run

data NotifyUrgencyHook = NotifyUrgencyHook
                       deriving (Read, Show)

instance UrgencyHook NotifyUrgencyHook where
    urgencyHook NotifyUrgencyHook w = do
        n      <- getName w
        Just i <- S.findTag w <$> gets windowset
        safeSpawn (C.notify C.applications) [show n, "workspace " ++ wrap "[" "]" i]

main :: IO ()
main = xmonad
       $ ewmh
       $ fullscreenSupport
       $ docks
       $ withUrgencyHook NotifyUrgencyHook
       $ withNavigation2DConfig C.navigation
       $ dynamicProjects C.projects
       $ def { borderWidth        = C.border
             , workspaces         = C.workspaces -- TODO save WS state
             , layoutHook         = C.layoutHook -- TODO save layout state and floating W position
             , terminal           = C.term C.applications
             , normalBorderColor  = C.colorN
             , focusedBorderColor = C.colorF
             , modMask            = C.modMask
             , keys               = C.keys
             , logHook            = C.logHook
             , startupHook        = C.startupHook
             , mouseBindings      = C.mouseBindings
             , manageHook         = C.manageHook
             , handleEventHook    = C.handleEventHook
             , focusFollowsMouse  = False
             , clickJustFocuses   = False
             }
