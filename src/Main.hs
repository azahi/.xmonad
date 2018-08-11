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
import qualified XMonad.Custom.Bindings         as Custom
import qualified XMonad.Custom.Event            as Custom
import qualified XMonad.Custom.Layout           as Custom
import qualified XMonad.Custom.Log              as Custom
import qualified XMonad.Custom.Manage           as Custom
import qualified XMonad.Custom.Misc             as Custom
import qualified XMonad.Custom.Navigation       as Custom
import qualified XMonad.Custom.Projects         as Custom
import qualified XMonad.Custom.Startup          as Custom
import qualified XMonad.Custom.Theme            as Custom
import qualified XMonad.Custom.Workspaces       as Custom
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import qualified XMonad.StackSet                as S
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run

data NotifyUrgencyHook = NotifyUrgencyHook
                       deriving (Read, Show)

instance UrgencyHook NotifyUrgencyHook where
    urgencyHook NotifyUrgencyHook w = do
        n      <- getName w
        Just i <- S.findTag w <$> gets windowset
        safeSpawn (Custom.notify Custom.customApplications) [show n, "workspace " ++ wrap "[" "]" i]

main :: IO ()
main = xmonad $ ewmh
              $ fullscreenSupport
              $ docks
              $ withUrgencyHook NotifyUrgencyHook
              $ withNavigation2DConfig Custom.navigation2DConfig
              $ dynamicProjects Custom.projects
              $ addDescrKeys' ((Custom.modMask', xK_F1), Custom.showKeyBindings) Custom.keyBindings
              $ def { borderWidth        = Custom.border
                    , workspaces         = Custom.workspaces' -- TODO save WS state
                    , layoutHook         = Custom.layoutHook' -- TODO save layout state and floating W position
                    , terminal           = Custom.term Custom.customApplications
                    , normalBorderColor  = Custom.colorN
                    , focusedBorderColor = Custom.colorF
                    , modMask            = Custom.modMask'
                    , logHook            = Custom.logHook'
                    , startupHook        = Custom.startupHook'
                    , mouseBindings      = Custom.mouseBindings'
                    , manageHook         = Custom.manageHook'
                    , handleEventHook    = Custom.handleEventHook'
                    , focusFollowsMouse  = False
                    , clickJustFocuses   = False
                    }
