-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Custom.Bindings
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-- Custom target for keyboard/mouse bindings.
--
------------------------------------------------------------------------

module XMonad.Custom.Bindings
    ( showKeyBindings
    , modMask'
    , keyBindings
    , mouseBindings'
    ) where

import           Control.Monad
import qualified Data.Map                            as M
import           System.Exit
import           System.IO
import           XMonad
import qualified XMonad.Actions.ConstrainedResize    as C
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate   as F
import           XMonad.Actions.FloatSnap
import           XMonad.Actions.FloatSnapSpaced
import           XMonad.Actions.MessageFeedback
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.PerConditionKeys
import           XMonad.Actions.Promote
import           XMonad.Actions.Volume
import           XMonad.Actions.WithAll
import           XMonad.Custom.Layout
import qualified XMonad.Custom.Misc                  as CM
import           XMonad.Custom.Scratchpads
import           XMonad.Custom.Theme
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Hidden
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SubLayouts
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet                     as S
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare
import           XMonad.Util.XSelection

showKeyBindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeyBindings a = addName "Show Keybindings" $ io $ do
    p <- spawnPipe "/usr/bin/zenity --text-info" -- TOOD Find an application that doesn't rely on any toolkits
    hPutStr p $ unlines $ showKm a
    hClose p
    return ()

modMask' :: KeyMask
modMask' = mod4Mask

directions :: [Direction2D]
directions = [D, U, L, R]

arrowKeys, directionKeys, wsKeys :: [String]
arrowKeys     = [ "<D>" , "<U>" , "<L>" , "<R>" ]
directionKeys = [  "j"  ,  "k"  ,  "h"  ,  "l"  ]
wsKeys        = map show [1..9 :: Int]

zipM  :: [a] -> String -> [[a]] -> [t] -> (t ->       X ()) ->       [([a], NamedAction)]
zipM  m nm ks as f   = zipWith (\k d -> (m ++ k, addName nm $ f d))   ks as
zipM' :: [a] -> String -> [[a]] -> [t] -> (t -> t1 -> X ()) -> t1 -> [([a], NamedAction)]
zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

tryMessageR_ :: (Message a, Message b) => a -> b -> X ()
tryMessageR_ x y = sequence_ [tryMessage_ x y, refresh]

xSelectionNotify :: MonadIO m => m ()
xSelectionNotify = join $ io
    $ (unsafeSpawn . (\x -> CM.notify CM.customApplications ++ " Clipboard " ++ wrap "\"\\\"" "\"\\\"" x)) <$> getSelection

toggleCopyToAll :: X ()
toggleCopyToAll = wsContainingCopies >>= \x -> case x of
                                                   [] -> windows copyToAll
                                                   _  -> killAllOtherCopies

getSortByIndexNonSP :: X ([WindowSpace] -> [WindowSpace])
getSortByIndexNonSP = (. namedScratchpadFilterOutWorkspace) <$> getSortByIndex

nextNonEmptyWS, prevNonEmptyWS :: X ()
nextNonEmptyWS = findWorkspace getSortByIndexNonSP Next HiddenNonEmptyWS 1 >>= \t -> windows . S.view $ t
prevNonEmptyWS = findWorkspace getSortByIndexNonSP Prev HiddenNonEmptyWS 1 >>= \t -> windows . S.view $ t

toggleFloat :: Window -> X ()
toggleFloat w = windows (\s -> if M.member w (S.floating s)
                               then S.sink w s
                                  else S.float w (S.RationalRect (1/2 - 1/4) (1/2 - 1/4) (1/2) (1/2)) s)

keyBindings :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
keyBindings c =
    let subKeys s ks = subtitle s:mkNamedKeymap c ks
    in
    subKeys "System"
    [ ("M-q"   , addName "Restart XMonad"             $ spawn "/usr/bin/xmonad --restart")
    , ("M-C-q" , addName "Recompile & restart XMonad" $ spawn "/usr/bin/xmonad --recompile && /usr/bin/xmonad --restart")
    , ("M-S-q" , addName "Quit XMonad"                $ confirmPrompt hotPromptTheme "Quit XMonad?" $ io exitSuccess)
    , ("M-x"   , addName "Shell prompt"               $ shellPrompt promptTheme)
    , ("M-o"   , addName "Goto W prompt"              $ windowPrompt promptTheme Goto allWindows)
    , ("M-S-o" , addName "Bring W prompt"             $ windowPrompt promptTheme Bring allWindows)
    ]
    ^++^
    subKeys "Actions"
    [ ("M-C-g"             , addName "Cancel"                                    $ return ())
    , ("<XF86ScreenSaver>" , addName "Lock screen"                               $ spawn "~/.xmonad/bin/\
                                                                                         \screenlock.sh")
    , ("M-S-c"             , addName "Print clipboard content"                     xSelectionNotify)
    , ("M-<Print>"         , addName "Take a screenshot of the current WS, \
                                     \upload it and copy link to the buffer"     $ spawn "~/.xmonad/bin/\
                                                                                         \xshot-upload.sh")
    , ("M-S-<Print>"       , addName "Take a screenshot of the selected area, \
                                     \upload it and copy link to the buffer"     $ spawn "~/.xmonad/bin/\
                                                                                         \xshot-select-upload.sh")
    , ("M-<Insert>"        , addName "Start recording screen as webm"            $ spawn "~/.xmonad/bin/\
                                                                                         \xcast.sh --webm")
    , ("M-S-<Insert>"      , addName "Start recording screen as gif"             $ spawn "~/.xmonad/bin/\
                                                                                         \xcast.sh --gif")
    , ("M-C-<Insert>"      , addName "Stop recording"                            $ spawn "/usr/bin/pkill ffmpeg")
    , ("M-C-c"             , addName "Toggle compton on/off"                     $ spawn "~/.xmonad/bin/\
                                                                                         \toggle-compton.sh")
    , ("M-C-r"             , addName "Toggle redshift on/off"                    $ spawn "~/.xmonad/bin/\
                                                                                         \toggle-redshift.sh")
    , ("M-C-p"             , addName "Toggle touchpad on/off"                    $ spawn "~/.xmonad/bin/\
                                                                                             \toggle-touchpad.sh")
    , ("M-C-t"             , addName "Toggle trackpoint on/off"                  $ spawn "~/.xmonad/bin/\
                                                                                         \toggle-trackpoint.sh")
    ]
    ^++^
    subKeys "Volume & Music"
    [ ("<XF86AudioMute>"        , addName "ALSA: Mute"         $ void   toggleMute)
    , ("<XF86AudioLowerVolume>" , addName "ALSA: Lower volume" $ void $ lowerVolume 5)
    , ("<XF86AudioRaiseVolume>" , addName "ALSA: Raise volume" $ void $ raiseVolume 5)
    , ("<XF86AudioPlay>"        , addName "MPD: Play/pause"    $ spawn "~/.xmonad/bin/mpc-play-pause.sh")
    , ("<XF86AudioStop>"        , addName "MPD: Stop"          $ spawn "/usr/bin/mpc --no-status stop")
    , ("<XF86AudioPrev>"        , addName "MPD: Previos track" $ spawn "/usr/bin/mpc --no-status prev")
    , ("<XF86AudioNext>"        , addName "MPD: Next track"    $ spawn "/usr/bin/mpc --no-status next")
    ]
    ^++^
    subKeys "Spawnables"
    [ ("M-<Return>" , addName "Terminal"         $ spawn (CM.term CM.customApplications))
    , ("M-b"        , addName "Browser"          $ spawn (CM.browser CM.customApplications))
    , ("M-S-p"      , addName "Pass prompt"      $ passPrompt promptTheme)
    , ("M-c"        , addName "NSP Console"      $ namedScratchpadAction scratchpads "console")
    , ("M-m"        , addName "NSP Music"        $ namedScratchpadAction scratchpads "music")
    , ("M-t"        , addName "NSP Top"          $ namedScratchpadAction scratchpads "top")
    , ("M-v"        , addName "NSP Volume"       $ namedScratchpadAction scratchpads "volume")
    ]
    ^++^
    subKeys "Windows"
    ( [ ("M-d"   , addName "Kill W"                     kill)
      , ("M-S-d" , addName "Kill all W on WS"         $ confirmPrompt hotPromptTheme "Kill all" killAll)
      , ("M-C-d" , addName "Duplicate W to all WS"      toggleCopyToAll)
      , ("M-a"   , addName "Hide W"                   $ withFocused hideWindow) -- FIXME This is so broken
      , ("M-S-a" , addName "Restore hidden W"           popOldestHiddenWindow)
      , ("M-p"   , addName "Promote W"                  promote)
      , ("M-s"   , addName "Merge W from sublayout"   $ withFocused $ sendMessage . MergeAll)
      , ("M-S-s" , addName "Unmerge W from sublayout" $ withFocused $ sendMessage . UnMerge)
      , ("M-u"   , addName "Focus urgent W"             focusUrgent)
      , ("M-e"   , addName "Focus master W"           $ windows S.focusMaster)
      , ("M-'"   , addName "Navigate tabbed W -> D"   $ bindOn LD [ ("Tabs" , windows S.focusDown)
                                                                    , (""     , onGroup S.focusDown')
                                                                    ])
      , ("M-;"   , addName "Navigate tabbed W -> U"   $ bindOn LD [ ("Tabs" , windows S.focusUp)
                                                                    , (""     , onGroup S.focusUp')
                                                                    ])
      , ("M-S-'" , addName "Swap tabbed W -> D"       $ windows S.swapDown)
      , ("M-S-;" , addName "Swap tabbed W -> U"       $ windows S.swapUp)
      ]
      ++ zipM' "M-"   "Navigate W"             directionKeys directions windowGo   True -- TODO W moving
      ++ zipM' "M-S-" "Swap W"                 directionKeys directions windowSwap True
      ++ zipM  "M-C-" "Merge W with sublayout" directionKeys directions (sendMessage . pullGroup)
      ++ zipM' "M-"   "Navigate screen"        arrowKeys directions screenGo       True
      ++ zipM' "M-S-" "Move W to screen"       arrowKeys directions windowToScreen True
      ++ zipM' "M-C-" "Swap W to screen"       arrowKeys directions screenSwap     True
    )
    ^++^
    subKeys "Workspaces & Projects"
    ( [ ("M-w"   , addName "Switch to project"     $ switchProjectPrompt  promptTheme)
      , ("M-S-w" , addName "Shift to project"      $ shiftToProjectPrompt promptTheme)
      , ("M-,"   , addName "Next non-empty WS"       nextNonEmptyWS)
      , ("M-."   , addName "Previous non-empty WS"   prevNonEmptyWS)
      , ("M-i"   , addName "Toggle last WS"        $ toggleWS' ["NSP"])
      , ("M-`"   , addName "WS prompt"             $ workspacePrompt promptTheme $ windows . S.shift)
      ]
      ++ zipM "M-"     "View WS"      wsKeys [0 ..] (withNthWorkspace S.greedyView)
      ++ zipM "M-S-"   "Move W to WS" wsKeys [0 ..] (withNthWorkspace S.shift)
      ++ zipM "M-C-S-" "Copy W to WS" wsKeys [0 ..] (withNthWorkspace copy)
    )
    ^++^
    subKeys "Layout Management"
    [ ("M-<Tab>"   , addName "Cycle layouts"            $ sendMessage NextLayout)
    , ("M-C-<Tab>" , addName "Cycle sublayouts"         $ toSubl NextLayout)
    , ("M-S-<Tab>" , addName "Reset layout"             $ setLayout $ XMonad.layoutHook c)
    , ("M-y"       , addName "Toggle float/tile on W"   $ withFocused toggleFloat)
    , ("M-S-y"     , addName "Tile all floating W"        sinkAll)
    , ("M-S-,"     , addName "Decrease maximum W count" $ sendMessage $ IncMasterN (-1))
    , ("M-S-."     , addName "Increase maximum W count" $ sendMessage $ IncMasterN 1)
    , ("M-r"       , addName "Rotate/reflect W"         $ tryMessageR_ Rotate (Toggle REFLECTX))
    , ("M-S-r"     , addName "Reflect W"                $ sendMessage $ Toggle REFLECTX)
    , ("M-f"       , addName "Toggle fullscreen layout" $ sequence_ [ withFocused $ windows . S.sink
                                                                    , sendMessage $ Toggle NBFULL
                                                                    ])
    , ("M-S-g"     , addName "Toggle gapped layout"     $ sendMessage $ Toggle GAPS) -- FIXME Breaks merged tabbed layout
    ]
    ^++^
    subKeys "Resize"
    [ ("M-["     , addName "Expand L" $ tryMessageR_ (ExpandTowards L) Shrink)
    , ("M-]"     , addName "Expand R" $ tryMessageR_ (ExpandTowards R) Expand)
    , ("M-S-["   , addName "Expand U" $ tryMessageR_ (ExpandTowards U) MirrorShrink)
    , ("M-S-]"   , addName "Expand D" $ tryMessageR_ (ExpandTowards D) MirrorExpand)
    , ("M-C-["   , addName "Shrink L" $ tryMessageR_ (ShrinkFrom    R) Shrink)
    , ("M-C-]"   , addName "Shrink R" $ tryMessageR_ (ShrinkFrom    L) Expand)
    , ("M-S-C-[" , addName "Shrink U" $ tryMessageR_ (ShrinkFrom    D) MirrorShrink)
    , ("M-S-C-]" , addName "Shrink D" $ tryMessageR_ (ShrinkFrom    U) MirrorExpand)
    ]

mouseBindings' :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' XConfig {XMonad.modMask = m} = M.fromList
    [ ((m,               button1), \w -> focus w
                                         >> F.mouseWindow F.position w
                                         >> ifClick (snapSpacedMagicMove gapBase  (Just 50) (Just 50) w)
                                         >> windows S.shiftMaster
      )
    , ((m .|. shiftMask, button1), \w -> focus w
                                         >> C.mouseResizeWindow w True
                                         >> ifClick (snapMagicResize [L, R, U, D] (Just 50) (Just 50) w)
                                         >> windows S.shiftMaster
      )
    , ((m,               button3), \w -> focus w
                                         >> F.mouseWindow F.linear w
                                         >> ifClick (snapMagicResize [L, R]       (Just 50) (Just 50) w)
                                         >> windows S.shiftMaster
      )
    , ((m .|. shiftMask, button3), \w -> focus w
                                         >> C.mouseResizeWindow w True
                                         >> ifClick (snapMagicResize [U, D]       (Just 50) (Just 50) w)
                                         >> windows S.shiftMaster
      )
    ]
