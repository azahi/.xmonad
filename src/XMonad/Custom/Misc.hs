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
-- Miscellaneous functions, data types and declarations used for 'XMonad.Custom'.
--
------------------------------------------------------------------------

module XMonad.Custom.Misc
    ( Applications (..)
    , customApplications
    ) where

data Applications = Applications
    { term    :: !String
    , browser :: !String
    , top     :: !String
    , mixer   :: !String
    , notify  :: !String
    } deriving (Eq, Show)

customApplications :: Applications
customApplications = Applications
    { term    = "/usr/bin/urxvtc"
    , browser = "/usr/bin/qutebrowser"
    , top     = "/usr/bin/htop"
    , mixer   = "/usr/bin/alsamixer"
    , notify  = "/usr/bin/notify-send"
    }
