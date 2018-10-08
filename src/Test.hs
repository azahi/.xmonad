module Main where

import Control.Monad (void)
import qualified Data.Map as M
import qualified Data.Set as S
import XMonad
import XMonad.Prompt
import XMonad.StackSet (new)
import XMonad.Util.EZConfig (checkKeymap)
import XMonad.Util.Font
import XMonad.Custom.Bindings (rawKeys)
import XMonad.Custom.Theme (promptTheme)

main :: IO ()
main = do
    dpy   <- openDisplay ""
    rootw <- rootWindow dpy $ defaultScreen dpy

    let xmc = def {layoutHook = Layout $ layoutHook def}
        initialWinset = new (layoutHook xmc) (workspaces xmc) []

    let cf = XConf { display       = dpy
                   , config        = xmc
                   , theRoot       = rootw
                   , normalBorder  = 0
                   , focusedBorder = 0
                   , keyActions    = M.empty
                   , buttonActions = M.empty
                   , mouseFocused  = False
                   , mousePosition = Nothing
                   , currentEvent  = Nothing
                   }

    let st = XState { windowset       = initialWinset
                    , numberlockMask  = 0
                    , mapped          = S.empty
                    , waitingUnmap    = M.empty
                    , dragging        = Nothing
                    , extensibleState = M.empty
                    }

    void $ runX cf st $ do
        checkKeymap xmc (rawKeys xmc)

        xmf <- initXMF (font promptTheme)

        case xmf of
            Core _ -> io (putStrLn "Font: core")
            Utf8 _ -> io (putStrLn "Font: utf8")
            Xft  _ -> io (putStrLn "Font: xft")
