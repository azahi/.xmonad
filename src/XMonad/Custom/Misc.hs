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
    { term    :: !String
    , browser :: !String
    , top     :: !String
    , mixer   :: !String
    , notify  :: !String
    } deriving (Eq, Show)

applications :: Applications
applications = Applications
    { term    = "urxvtc"
    , browser = "qutebrowser"
    , top     = "htop"
    , mixer   = "alsamixer"
    , notify  = "notify-send"
    }
