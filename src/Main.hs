{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Format
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           XMonad                              hiding ((|||))
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
import           XMonad.Actions.SinkAll
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Volume
import           XMonad.Actions.WithAll
import           XMonad.Hooks.CurrentWorkspaceOnTop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops           hiding
                                                      (fullscreenEventHook)
import           XMonad.Hooks.FloatNext
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Accordion
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.ComboP
import           XMonad.Layout.Decoration
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.Hidden
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet                     as S
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Loggers.NamedScratchpad
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows
import           XMonad.Util.PositionStore
import           XMonad.Util.Run
import           XMonad.Util.SpawnNamedPipe
import           XMonad.Util.WorkspaceCompare
import           XMonad.Util.XSelection

(+++) :: String -> String -> String
(+++) (x:xs) ys = x:xs ++ " " ++ ys
(+++) []     ys = ys

myBorderWidth :: Dimension
myBorderWidth = 1

myBrowser, myNotify, myTerminal :: String -- TODO Pull values with environment variables
myBrowser  = "/usr/bin/qutebrowser"
myNotify   = "/usr/bin/notify-send"
myTerminal = "/usr/bin/urxvtc"

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings a = addName "Show Keybindings" $ io $ do
    p <- spawnPipe "/usr/bin/zenity --text-info" -- TOOD Find an application that doesn't rely on any toolkits
    hPutStr p $ unlines $ showKm a
    hClose p
    return ()

data LibnotifyUrgencyHook = LibnotifyUrgencyHook
                          deriving (Read, Show)

instance UrgencyHook LibnotifyUrgencyHook where
    urgencyHook LibnotifyUrgencyHook w = do
        n      <- getName w
        Just i <- S.findTag w <$> gets windowset
        safeSpawn myNotify [show n, "workspace" +++ wrap "[" "]" i]

main :: IO ()
main = xmonad $ ewmh
              $ fullscreenSupport
              $ docks
              $ withUrgencyHook LibnotifyUrgencyHook
              $ withNavigation2DConfig myNav2DConf
              $ dynamicProjects myProjects
              $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
                myXMonadConfig -- TODO Make selection target the last selected W when changing scopes

myNav2DConf :: Navigation2DConfig
myNav2DConf = def
    { defaultTiledNavigation = hybridNavigation
    , floatNavigation        = hybridNavigation
    , layoutNavigation       = [("Full", centerNavigation)]
    , unmappedWindowRect     = [("Full", singleWindowRect)]
    }

myXMonadConfig = def
    { XMonad.borderWidth        = myBorderWidth
    , XMonad.workspaces         = myWorkspaces -- TODO save WS state
    , XMonad.layoutHook         = myLayoutHook -- TODO save layout state and floating W position
    , XMonad.terminal           = myTerminal
    , XMonad.normalBorderColor  = myColorN
    , XMonad.focusedBorderColor = myColorF
    , XMonad.modMask            = myModMask
    , XMonad.logHook            = myLogHook
    , XMonad.startupHook        = myStartupHook
    , XMonad.mouseBindings      = myMouseBindings
    , XMonad.manageHook         = myManageHook
    , XMonad.handleEventHook    = myHandleEventHook
    , XMonad.focusFollowsMouse  = myFocusFollowsMouse
    , XMonad.clickJustFocuses   = myClickJustFocuses
    }

myFocusFollowsMouse, myClickJustFocuses :: Bool
myFocusFollowsMouse = False
myClickJustFocuses  = False

wsCHAT = "CHA" :: String
wsM1   = "M/1" :: String
wsM2   = "M/2" :: String
wsM3   = "M/3" :: String
wsT    = "TER" :: String
wsW1   = "W/1" :: String
wsW2   = "W/2" :: String
wsW3   = "W/3" :: String
wsWWW  = "WWW" :: String

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int]

myProjects :: [Project]
myProjects =
    [ Project { projectName      = "Template"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }

    , Project { projectName      = wsW1
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawnOn wsW1 myTerminal
                                             spawnOn wsW1 myTerminal
              }

    , Project { projectName      = wsW2
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawnOn wsW2 myTerminal
                                             spawnOn wsW2 myTerminal
              }

    , Project { projectName      = wsW3
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawnOn wsW3 myTerminal
                                             spawnOn wsW3 myTerminal
              }

    , Project { projectName      = wsWWW
              , projectDirectory = "~/"
              , projectStartHook = Just $    spawnOn wsWWW myBrowser
              }
    ]

myFont :: String
myFont = "xft:lucy tewi:style=Regular:size=8" -- TODO CJKのフォールバックフォントを追加する

black1   = "#0b0806" :: String -- TODO get variables from Xresources
black2   = "#2f2b2a" :: String
red1     = "#844d2c" :: String
red2     = "#a64848" :: String
green1   = "#57553a" :: String
green2   = "#897f5a" :: String
yellow1  = "#a17c38" :: String
yellow2  = "#c8b38d" :: String
blue1    = "#41434f" :: String
blue2    = "#526274" :: String
magenta1 = "#6b4444" :: String
magenta2 = "#755c47" :: String
cyan1    = "#59664c" :: String
cyan2    = "#718062" :: String
white1   = "#a19782" :: String
white2   = "#c1ab83" :: String

myColorN, myColorF :: String
myColorN = black2
myColorF = white2

baseInt, fullInt :: Int
baseInt = 12
fullInt = baseInt * 2

fullDim :: Dimension
fullDim = 12 * 2

myTabTheme :: Theme
myTabTheme = def
    { activeColor         = black1
    , inactiveColor       = black2
    , urgentColor         = red1
    , activeBorderColor   = white1
    , inactiveBorderColor = white2
    , urgentBorderColor   = red2
    , activeTextColor     = white1
    , inactiveTextColor   = white2
    , urgentTextColor     = red2
    , fontName            = myFont
    , decoHeight          = fullDim
    }

myPromptTheme, myHotPromptTheme :: XPConfig
myPromptTheme = def
    { font              = myFont
    , bgColor           = black1
    , fgColor           = white1
    , fgHLight          = white2
    , bgHLight          = black2
    , borderColor       = white2
    , promptBorderWidth = myBorderWidth
    , position          = Bottom
    , height            = fullDim
    , searchPredicate   = isInfixOf `on` map toLower
    }
myHotPromptTheme = myPromptTheme
    { bgColor           = black2
    , fgColor           = white2
    , fgHLight          = white1
    , bgHLight          = black1
    }

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacing baseInt

myGaps :: l a -> ModifiedLayout Gaps l a
myGaps = gaps [ (U, baseInt)
              , (D, baseInt)
              , (R, baseInt)
              , (L, baseInt)
              ]

data MyTransformers = GAPS
                    deriving (Read, Show, Eq, Typeable)

instance Transformer MyTransformers Window where
    transform GAPS x k = k (avoidStruts $ myGaps $ mySpacing x) (const x)

myLayoutHook = fullscreenFloat
             $ lessBorders OnlyFloat
             $ mkToggle (single NBFULL)
             $ avoidStruts
             $ mkToggle (single GAPS)
             $ mkToggle (single REFLECTX)
             $ mkToggle (single REFLECTY)
             $ windowNavigation
             $ addTabs shrinkText myTabTheme
             $ hiddenWindows
             $ subLayout [] (Simplest ||| Accordion) -- TODO Proper bindings
               emptyBSP

myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys config = let
    subKeys :: String -> [(String, NamedAction)] -> [((KeyMask, KeySym), NamedAction)]
    subKeys s ks = subtitle s:mkNamedKeymap config ks

    directions :: [Direction2D]
    directions = [D, U, L, R]

    arrowKeys, directionKeys, wsKeys :: [String]
    arrowKeys     = [ "<D>" , "<U>" , "<L>" , "<R>" ]
    directionKeys = [  "j"  ,  "k"  ,  "h"  ,  "l"  ]
    wsKeys        = map show [1 .. 9 :: Int]

    zipM  :: [a] -> String -> [[a]] -> [t] -> (t ->       X ()) ->       [([a], NamedAction)]
    zipM  m nm ks as f   = zipWith (\k d -> (m ++ k, addName nm $ f d))   ks as
    zipM' :: [a] -> String -> [[a]] -> [t] -> (t -> t1 -> X ()) -> t1 -> [([a], NamedAction)]
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    tryMessageR_ :: (Message a, Message b) => a -> b -> X ()
    tryMessageR_ x y = sequence_ [tryMessage_ x y, refresh]

    unsafeSelectionNotify :: MonadIO m => String -> m ()
    unsafeSelectionNotify a = join
                            $ io
                            $ (unsafeSpawn . (\x -> a +++ "Clipboard" +++ wrap "\"\\\"" "\"\\\"" x))
                              <$> getSelection

    toggleCopyToAll :: X ()
    toggleCopyToAll = wsContainingCopies >>= \x -> case x of
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
    toggleFloat w = windows (\s -> if M.member w (S.floating s)
                            then S.sink w s
                            else S.float w (S.RationalRect (1/2 - 1/4) (1/2 - 1/4) (1/2) (1/2)) s)

    in

    subKeys "System"
    [ ("M-q"    , addName "Restart XMonad"             $ spawn "/usr/bin/xmonad --restart")
    , ("M-C-q"  , addName "Recompile & restart XMonad" $ spawn "/usr/bin/xmonad --recompile && /usr/bin/xmonad --restart")
    , ("M-S-q"  , addName "Quit XMonad"                $ confirmPrompt myHotPromptTheme "Quit XMonad?" $ io exitSuccess)
    , ("M-x"    , addName "Shell prompt"               $ shellPrompt myPromptTheme)
    , ("M-o"    , addName "Goto W prompt"              $ windowPrompt myPromptTheme Goto allWindows)
    , ("M-S-o"  , addName "Bring W prompt"             $ windowPrompt myPromptTheme Bring allWindows)
    ]

    ^++^
    subKeys "Actions"
    [ ("M-C-g"             , addName "Cancel"                                    $ return ())
    , ("<XF86ScreenSaver>" , addName "Lock screen"                               $ spawn "~/.xmonad/bin/\
                                                                                         \screenlock.sh")
    , ("M-S-c"             , addName "Print clipboard content"                   $ unsafeSelectionNotify myNotify)
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
    subKeys "Volume & Music" -- TODO replace play/pause script with Haskell implementation
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
    [ ("M-<Return>" , addName "Terminal"         $ spawn myTerminal)
    , ("M-b"        , addName "Browser"          $ spawn myBrowser)
    , ("M-S-p"      , addName "Pass prompt"      $ passPrompt myPromptTheme)
    , ("M-c"        , addName "NSP Console"      $ namedScratchpadAction myScratchpads "console")
    , ("M-m"        , addName "NSP Music"        $ namedScratchpadAction myScratchpads "music")
    , ("M-t"        , addName "NSP Top"          $ namedScratchpadAction myScratchpads "top")
    , ("M-v"        , addName "NSP Volume"       $ namedScratchpadAction myScratchpads "volume")
    ]

    ^++^
    subKeys "Windows"
    ( [ ("M-d"     , addName "Kill W"                     kill)
      , ("M-S-d"   , addName "Kill all W on WS"         $ confirmPrompt myHotPromptTheme "Kill all" killAll)
      , ("M-C-d"   , addName "Duplicate W to all WS"      toggleCopyToAll)
      , ("M-a"     , addName "Hide W"                   $ withFocused hideWindow) -- FIXME This is so broken
      , ("M-S-a"   , addName "Restore hidden W"           popOldestHiddenWindow)
      , ("M-p"     , addName "Promote W"                  promote)
      , ("M-s"     , addName "Merge W from sublayout"   $ withFocused $ sendMessage . MergeAll)
      , ("M-S-s"   , addName "Unmerge W from sublayout" $ withFocused $ sendMessage . UnMerge)
      , ("M-u"     , addName "Focus urgent W"             focusUrgent)
      , ("M-e"     , addName "Focus master W"           $ windows S.focusMaster)
      , ("M-'"     , addName "Navigate tabbed W -> D"   $ bindOn LD [ ("Tabs" , windows S.focusDown)
                                                                    , (""     , onGroup S.focusDown')
                                                                    ])
      , ("M-;"     , addName "Navigate tabbed W -> U"   $ bindOn LD [ ("Tabs" , windows S.focusUp)
                                                                    , (""     , onGroup S.focusUp')
                                                                    ])
      , ("M-S-'"   , addName "Swap tabbed W -> D"       $ windows S.swapDown)
      , ("M-S-;"   , addName "Swap tabbed W -> U"       $ windows S.swapUp)
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
    ( [ ("M-w"   , addName "Switch to project"     $ switchProjectPrompt  myPromptTheme)
      , ("M-S-w" , addName "Shift to project"      $ shiftToProjectPrompt myPromptTheme)
      , ("M-,"   , addName "Next non-empty WS"       nextNonEmptyWS)
      , ("M-."   , addName "Previous non-empty WS"   prevNonEmptyWS)
      , ("M-i"   , addName "Toggle last WS"        $ toggleWS' ["NSP"])
      , ("M-`"   , addName "WS prompt"             $ workspacePrompt myPromptTheme $ windows . S.shift)
      ]

      ++ zipM "M-"     "View WS"      wsKeys [0 ..] (withNthWorkspace S.greedyView)
      ++ zipM "M-S-"   "Move W to WS" wsKeys [0 ..] (withNthWorkspace S.shift)
      ++ zipM "M-C-S-" "Copy W to WS" wsKeys [0 ..] (withNthWorkspace copy)
    )

    ^++^
    subKeys "Layout Management"
    [ ("M-<Tab>"   , addName "Cycle layouts"            $ sendMessage NextLayout)
    , ("M-C-<Tab>" , addName "Cycle sublayouts"         $ toSubl NextLayout)
    , ("M-S-<Tab>" , addName "Reset layout"             $ setLayout $ XMonad.layoutHook config)
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

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "console"
         (spawnTerminalWith "NSPConsole" "~/.xmonad/bin/nsp-console.sh")
         (title =? "NSPConsole")
         floatingNSP
    , NS "volume"
         (spawnTerminalWith "NSPVolume" "/usr/bin/alsamixer")
         (title =? "NSPVolume")
         floatingNSP
    , NS "music"
         (spawnTerminalWith "NSPMusic" "~/.bin/mp")
         (title =? "NSPMusic")
         floatingNSP
    , NS "top"
         (spawnTerminalWith "NSPTop" "/usr/bin/htop")
         (title =? "NSPTop")
         floatingNSP
    ]
    where
        spawnTerminalWith :: String -> String -> String
        spawnTerminalWith t c = myTerminal +++ "-title" +++ t +++ "-e" +++ c

        floatingNSP :: ManageHook
        floatingNSP = customFloating $ S.RationalRect x y w h
        x = (1 - w) / 2
        y = (1 - h) / 2
        w = 1 / 2
        h = 1 / 2.5

xmobarFont :: Int -> String -> String
xmobarFont fn = wrap (concat ["<fn=", show fn, ">"]) "</fn>"

myTopBarPP :: PP
myTopBarPP = def
    { ppCurrent         = xmobarColor white2 "" . xmobarFont 2 . wrap "=" "="
    , ppVisible         = xmobarColor white1 ""                . wrap "~" "~"
    , ppHidden          = xmobarColor white1 ""                . wrap "-" "-"
    , ppHiddenNoWindows = xmobarColor white1 ""                . wrap "_" "_"
    , ppUrgent          = xmobarColor red1   ""                . wrap "!" "!"
    , ppSep             = " / "
    , ppWsSep           = " "
    , ppTitle           = xmobarColor white1 "" . shorten 50
    , ppTitleSanitize   = xmobarStrip
    , ppLayout          = xmobarColor white1 "" . \x -> case x of
                                                             "Spacing 12 Tabbed Hidden BSP" -> "Omni.Gaps"
                                                             "Tabbed Hidden BSP"            -> "Omni"
                                                             _                              -> "Misc"
    , ppOrder           = id
    , ppSort            = (namedScratchpadFilterOutWorkspace .) <$> getSortByIndex
    , ppExtras          = []
    }

myBotBarPP :: PP
myBotBarPP = myTopBarPP
    { ppCurrent         = const ""
    , ppVisible         = const ""
    , ppHidden          = const ""
    , ppHiddenNoWindows = const ""
    , ppUrgent          = const ""
    , ppTitle           = const ""
    , ppLayout          = const ""
    }

myLogHook :: X ()
myLogHook = do
    currentWorkspaceOnTop
    ewmhDesktopsLogHook
    b1 <- getNamedPipe "xmobarTop"
    b2 <- getNamedPipe "xmobarBot"
    c  <- wsContainingCopies
    let copiesCurrent ws | ws `elem` c = xmobarColor yellow2 "" . xmobarFont 2 . wrap "*" "=" $ ws
                         | otherwise   = xmobarColor white2  "" . xmobarFont 2 . wrap "=" "=" $ ws
    let copiesHidden  ws | ws `elem` c = xmobarColor yellow1 ""                . wrap "*" "-" $ ws
                         | otherwise   = xmobarColor white1  ""                . wrap "-" "-" $ ws
    let copiesUrgent  ws | ws `elem` c = xmobarColor yellow2 ""                . wrap "*" "!" $ ws
                         | otherwise   = xmobarColor white2  ""                . wrap "!" "!" $ ws
    dynamicLogWithPP $ myTopBarPP
        { ppCurrent = copiesCurrent
        , ppHidden  = copiesHidden
        , ppUrgent  = copiesUrgent
        , ppOutput  = safePrintToPipe b1
        }
    dynamicLogWithPP $ myBotBarPP
        { ppOutput  = safePrintToPipe b2
        }
    where
        safePrintToPipe :: Maybe Handle -> String -> IO ()
        safePrintToPipe = maybe (\_ -> return ()) hPutStrLn

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \d -> do
    r  <- asks theRoot
    ns <- getAtom "_NET_SUPPORTED"
    a  <- getAtom "ATOM"
    liftIO $ do
        s <- (join . maybeToList) <$> getWindowProperty32 d ns r
        when (fromIntegral x `notElem` s) $ changeProperty32 d r ns a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
    s <- mapM getAtom atoms
    mapM_ addNETSupported s
    where
        atoms :: [String]
        atoms = [ "_NET_ACTIVE_WINDOW"
                , "_NET_CLIENT_LIST"
                , "_NET_CLIENT_LIST_STACKING"
                , "_NET_DESKTOP_NAMES"
                , "_NET_WM_DESKTOP"
                , "_NET_WM_STATE"
                , "_NET_WM_STATE_FULLSCREEN"
                , "_NET_WM_STATE_HIDDEN"
                , "_NET_WM_STRUT"
                ]

myStartupHook :: X ()
myStartupHook = do
    startupHook def
    spawnNamedPipe "/usr/bin/xmobar ~/.xmonad/xmobarrcTop.hs" "xmobarTop"
    spawnNamedPipe "/usr/bin/xmobar ~/.xmonad/xmobarrcBot.hs" "xmobarBot"
    nspTrackStartup myScratchpads -- TODO
    docksStartupHook
    addEWMHFullscreen
    setDefaultCursor xC_left_ptr
    setWMName "xmonad"

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ()) -- TODO implement snapSpacedMagicResize
myMouseBindings XConfig {XMonad.modMask = m} = M.fromList
    [ ((m,               button1), \w -> focus w >> F.mouseWindow F.position w
                                                 >> ifClick (snapSpacedMagicMove fullInt  (Just 50) (Just 50) w)
                                                 >> windows S.shiftMaster
      )
    , ((m .|. shiftMask, button1), \w -> focus w >> C.mouseResizeWindow w True
                                                 >> ifClick (snapMagicResize [L, R, U, D] (Just 50) (Just 50) w)
                                                 >> windows S.shiftMaster
      )
    , ((m,               button3), \w -> focus w >> F.mouseWindow F.linear w
                                                 >> ifClick (snapMagicResize [L, R]       (Just 50) (Just 50) w)
                                                 >> windows S.shiftMaster
      )
    , ((m .|. shiftMask, button3), \w -> focus w >> C.mouseResizeWindow w True
                                                 >> ifClick (snapMagicResize [U, D]       (Just 50) (Just 50) w)
                                                 >> windows S.shiftMaster
      )
    ]

myManageHook :: ManageHook
myManageHook = manageHook def
           <+> manageDocks
           <+> fullscreenManageHook
           <+> namedScratchpadManageHook myScratchpads
           <+> composeOne composeActions
    where
        composeActions :: [MaybeManageHook]
        composeActions = [ isFullscreen                                       -?> doFullFloat
                         , className =? "explorer.exe"                        -?> doFullFloat
                         , isDialog                                           -?> doCenterFloat
                         , className =? "Xmessage"                            -?> doCenterFloat
                         , className =? "Zenity"                              -?> doCenterFloat
                         , className =? "qemu-system-x86"                     -?> doCenterFloat
                         , className =? "qemu-system-x86_64"                  -?> doCenterFloat
                         , className =? "Steam" <&&> not <$> title =? "Steam" -?> doFloat
                         ]

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def
                <+> nspTrackHook myScratchpads
                <+> docksEventHook
                <+> fullscreenEventHook

-- vim:filetype=haskell:expandtab:tabstop=4:shiftwidth=4
