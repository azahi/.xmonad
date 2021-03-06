{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : XMonad.Custom.Bindings
-- Copyright   : (c) 2018-2020 Azat Bahawi <azahi@teknik.io>
-- License     : BSD3-style (see LICENSE)
-- Maintainer  : Azat Bahawi <azahi@teknik.io>
-- Stability   : unstable
-- Portability : unportable
--

module XMonad.Custom.Bindings
  ( keys
  , rawKeys
  , modMask
  , mouseBindings
  ) where

import qualified Data.Map                      as M
import           System.Exit
import           XMonad                  hiding ( keys
                                                , modMask
                                                , mouseBindings
                                                )
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate
                                               as F
import           XMonad.Actions.FloatSnap
import           XMonad.Actions.FloatSnapSpaced
import           XMonad.Actions.MessageFeedback
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.PerConditionKeys
import           XMonad.Actions.Promote
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WithAll
import           XMonad.Custom.Layout
import qualified XMonad.Custom.Misc            as C
import           XMonad.Custom.Scratchpads
import           XMonad.Custom.Theme
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Hidden
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SubLayouts
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet               as S
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.WorkspaceCompare

modMask :: KeyMask
modMask = mod4Mask

directions :: [Direction2D]
directions = [D, U, L, R]

arrowKeys, directionKeys, wsKeys :: [String]
arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
directionKeys = ["j", "k", "h", "l"]
wsKeys = map show [1 .. 9 :: Int]

zipKeys :: [a] -> [[a]] -> [t1] -> (t1 -> b) -> [([a], b)]
zipKeys m ks as f = zipWith (\k d -> (m ++ k, f d)) ks as
zipKeys' :: [a] -> [[a]] -> [t1] -> (t1 -> t2 -> b) -> t2 -> [([a], b)]
zipKeys' m ks as f b = zipWith (\k d -> (m ++ k, f d b)) ks as

tryMessageR_ :: (Message a, Message b) => a -> b -> X ()
tryMessageR_ x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]

toggleCopyToAll :: X ()
toggleCopyToAll = wsContainingCopies >>= \case
  [] -> windows copyToAll
  _  -> killAllOtherCopies

getSortByIndexNonSP :: X ([WindowSpace] -> [WindowSpace])
getSortByIndexNonSP = (. namedScratchpadFilterOutWorkspace) <$> getSortByIndex

nextNonEmptyWS, prevNonEmptyWS :: X ()
nextNonEmptyWS = findWorkspace getSortByIndexNonSP Next HiddenNonEmptyWS 1
  >>= \t -> windows . S.view $ t
prevNonEmptyWS = findWorkspace getSortByIndexNonSP Prev HiddenNonEmptyWS 1
  >>= \t -> windows . S.view $ t

toggleFloat :: Window -> X ()
toggleFloat w = windows
  (\s -> if M.member w (S.floating s)
    then S.sink w s
    else S.float
      w
      (S.RationalRect (1 / 2 - 1 / 4) (1 / 2 - 1 / 4) (1 / 2) (1 / 2))
      s
  )

withUpdatePointer :: [(String, X ())] -> [(String, X ())]
withUpdatePointer = map addAction
 where
  addAction :: (String, X ()) -> (String, X ())
  addAction (key, action) = (key, action >> updatePointer (0.98, 0.01) (0, 0))

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys c = mkKeymap c (rawKeys c)

rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = withUpdatePointer $ concatMap ($ c) keymaps
 where
  keymaps =
    [ keysBase
    , keysSystem
    , keysMedia
    , keysWorkspaces
    , keysSpawnables
    , keysWindows
    , keysLayout
    , keysResize
    ]

keysBase :: XConfig Layout -> [(String, X ())]
keysBase _ =
  [ ("M-S-q", confirmPrompt hotPromptTheme "Quit XMonad?" $ io exitSuccess)
  , ("M-q"  , spawn "xmonad --restart") -- TODO Replace with interal calls
  , ("M-C-q", spawn "xmonad --recompile && xmonad --restart")
  , ("M-x"  , shellPrompt promptTheme)
  , ("M-w"  , windowPrompt promptTheme Goto allWindows)
  , ("M-S-w", windowPrompt promptTheme Bring allWindows)
  ]

keysSystem :: XConfig Layout -> [(String, X ())]
keysSystem _ =
  [ ("M-C-g"            , return ()) -- TODO Replace scripts with internal functions
  , ("<XF86ScreenSaver>", spawn "~/.xmonad/scripts/screenlock.sh")
  , ("M-<Print>", spawn "~/.xmonad/scripts/xshot-upload.sh")
  , ("M-S-<Print>", spawn "~/.xmonad/scripts/xshot-select-upload.sh")
  , ("M-C-c", spawn "~/.xmonad/scripts/toggle-compton.sh")
  , ("M-C-r", spawn "~/.xmonad/scripts/toggle-redshift.sh")
  , ("M-C-p", spawn "~/.xmonad/scripts/toggle-touchpad.sh")
  , ("M-C-t", spawn "~/.xmonad/scripts/toggle-trackpoint.sh")
  ]

keysMedia :: XConfig Layout -> [(String, X ())] -- TODO Make audio keys compatible with ALSA/PA at the same time
keysMedia _ =
  [ ("<XF86AudioMicMute>", spawn "pactl set-source-mute 1 toggle")
  , ("<XF86AudioMute>"   , spawn "pactl set-sink-mute 0 toggle")
  , ( "<XF86AudioLowerVolume>"
    , spawn "pactl set-sink-mute 0 false && pactl set-sink-volume 0 -10%"
    )
  , ( "<XF86AudioRaiseVolume>"
    , spawn "pactl set-sink-mute 0 false && pactl set-sink-volume 0 +10%"
    )
  , ("<XF86AudioPlay>", spawn "mpc toggle")
  , ("<XF86AudioStop>", spawn "mpc stop")
  , ("<XF86AudioPrev>", spawn "mpc prev")
  , ("<XF86AudioNext>", spawn "mpc next")
  ]

keysWorkspaces :: XConfig Layout -> [(String, X ())]
keysWorkspaces _ =
  [ ("M-S-o", switchProjectPrompt promptTheme)
    , ("M-S-p", shiftToProjectPrompt promptTheme)
    , ("M-,"  , nextNonEmptyWS)
    , ("M-."  , prevNonEmptyWS)
    , ("M-i"  , toggleWS' ["NSP"])
    , ("M-n", workspacePrompt promptTheme $ windows . S.shift)
    ]
    ++ zipKeys "M-"     wsKeys [0 ..] (withNthWorkspace S.greedyView)
    ++ zipKeys "M-S-"   wsKeys [0 ..] (withNthWorkspace S.shift)
    ++ zipKeys "M-C-S-" wsKeys [0 ..] (withNthWorkspace copy)

keysSpawnables :: XConfig Layout -> [(String, X ())]
keysSpawnables _ =
  [ ("M-<Return>", spawn (C.term C.applications))
  , ("M-b"       , spawn (C.browser C.applications))
  , ("M-c"       , namedScratchpadAction scratchpads "console")
  , ("M-m"       , namedScratchpadAction scratchpads "music")
  , ("M-t"       , namedScratchpadAction scratchpads "top")
  , ("M-v"       , namedScratchpadAction scratchpads "volume")
  ]

keysWindows :: XConfig Layout -> [(String, X ())]
keysWindows _ =
  [ ("M-d"  , kill)
    , ("M-S-d", confirmPrompt hotPromptTheme "Kill all" killAll)
    , ("M-a"  , toggleCopyToAll)
    , ("M-e"  , withFocused hideWindow) -- FIXME This is so broken
    , ("M-S-e", popOldestHiddenWindow)
    , ("M-p"  , promote)
    , ("M-g", withFocused $ sendMessage . MergeAll)
    , ("M-S-g", withFocused $ sendMessage . UnMerge)
    , ("M-u"  , focusUrgent)
    , ("M-s"  , windows S.focusMaster)
    , ( "M-'"
      , bindOn LD [("Tabs", windows S.focusDown), ("", onGroup S.focusDown')]
      )
    , ("M-;", bindOn LD [("Tabs", windows S.focusUp), ("", onGroup S.focusUp')])
    , ("M-S-'", windows S.swapDown)
    , ("M-S-;", windows S.swapUp)
    ]
    ++ zipKeys' "M-"   directionKeys directions windowGo   True -- TODO W moving
    ++ zipKeys' "M-S-" directionKeys directions windowSwap True
    ++ zipKeys "M-C-" directionKeys directions (sendMessage . pullGroup)
    ++ zipKeys' "M-"   arrowKeys directions screenGo       True
    ++ zipKeys' "M-S-" arrowKeys directions windowToScreen True
    ++ zipKeys' "M-C-" arrowKeys directions screenSwap     True

keysLayout :: XConfig Layout -> [(String, X ())]
keysLayout c =
  [ ("M-<Tab>"  , sendMessage NextLayout)
  , ("M-C-<Tab>", toSubl NextLayout)
  , ("M-S-<Tab>", setLayout $ XMonad.layoutHook c)
  , ("M-o"      , withFocused toggleFloat)
  , ("M-S-o"    , sinkAll)
  , ("M-S-,"    , sendMessage $ IncMasterN (-1))
  , ("M-S-."    , sendMessage $ IncMasterN 1)
  , ("M-r"      , tryMessageR_ Rotate (Toggle REFLECTX))
  , ("M-S-r"    , sendMessage $ Toggle REFLECTX)
  , ( "M-f"
    , sequence_ [withFocused $ windows . S.sink, sendMessage $ Toggle NBFULL]
    )
  , ("M-C-g", sendMessage $ Toggle GAPS) -- FIXME Breaks merged tabbed layout
  ]

keysResize :: XConfig Layout -> [(String, X ())]
keysResize _ =
  [ ("M-["    , tryMessageR_ (ExpandTowards L) Shrink)
  , ("M-]"    , tryMessageR_ (ExpandTowards R) Expand)
  , ("M-S-["  , tryMessageR_ (ExpandTowards U) MirrorShrink)
  , ("M-S-]"  , tryMessageR_ (ExpandTowards D) MirrorExpand)
  , ("M-C-["  , tryMessageR_ (ShrinkFrom R) Shrink)
  , ("M-C-]"  , tryMessageR_ (ShrinkFrom L) Expand)
  , ("M-S-C-[", tryMessageR_ (ShrinkFrom D) MirrorShrink)
  , ("M-S-C-]", tryMessageR_ (ShrinkFrom U) MirrorExpand)
  ]

mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings XConfig{} = M.fromList
  [ ( (modMask, button1)
    , \w ->
      focus w
        >> F.mouseWindow F.position w
        >> ifClick (snapSpacedMagicMove gapFull (Just 50) (Just 50) w)
        >> windows S.shiftMaster
    )
  , ( (modMask, button3)
    , \w ->
      focus w
        >> F.mouseWindow F.linear w
        >> ifClick (snapMagicResize [L, R, U, D] (Just 50) (Just 50) w)
        >> windows S.shiftMaster
    )
  ]
