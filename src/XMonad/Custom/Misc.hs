{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Custom.Misc
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Custom.Misc
    ( Applications (..)
    , applications
    ) where

data Applications = Applications
    { browser :: !String
    , mixer   :: !String
    , notify  :: !String
    , player  :: !String
    , term    :: !String
    , top     :: !String
    } deriving (Eq, Show)

applications :: Applications
applications = Applications
    { browser = "qutebrowser"
    , mixer   = "alsamixer"
    , notify  = "notify-send"
    , player  = "ncmpcpp"
    , term    = "urxvtc"
    , top     = "htop"
    }
