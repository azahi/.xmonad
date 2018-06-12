module XMonad.Custom.Manage
    ( manageHook'
    ) where

import           XMonad
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
import           XMonad.Util.NamedScratchpad

composeActions :: [MaybeManageHook]
composeActions =
    [ appName =? "emacs-popup"                                  -?> tileBelowNoFocus
    , appName =? "eterm"                                        -?> tileBelow
    , className =? "Pinentry"                                   -?> doCenterFloat
    , className =? "Steam" <&&> not <$> title =? "Steam"        -?> doFloat
    , className =? "URxvt"                                      -?> tileBelow
    , className =? "Xmessage"                                   -?> doCenterFloat
    , className =? "Zenity"                                     -?> doCenterFloat
    , className =? "explorer.exe"                               -?> doFullFloat
    , className =? "qemu-system-x86"                            -?> doCenterFloat
    , className =? "qemu-system-x86_64"                         -?> doCenterFloat
    , className =? "xterm"                                      -?> tileBelow
    , isDialog                                                  -?> doCenterFloat
    , isFullscreen                                              -?> doFullFloat
    , pure True                                                 -?> normalTile
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up"               -?> doCenterFloat
    , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" -?> doCenterFloat
    , transience
    ]
    where
        normalTile       = insertPosition Above Newer
        tileBelow        = insertPosition Below Newer
        tileBelowNoFocus = insertPosition Below Older


manageHook' :: ManageHook
manageHook' = mconcat [ manageDocks
                      , fullscreenManageHook
                      , namedScratchpadManageHook scratchpads
                      , composeOne composeActions
                      ]
