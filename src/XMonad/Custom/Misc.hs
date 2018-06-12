module XMonad.Custom.Misc
    ( Applications (..)
    , customApplications
    ) where

data Applications = Applications { term    :: String
                                 , browser :: String
                                 , top     :: String
                                 , mixer   :: String
                                 , notify  :: String
                                 } deriving (Show)

customApplications :: Applications
customApplications = Applications { term    = "/usr/bin/urxvtc"
                                  , browser = "/usr/bin/qutebrowser"
                                  , top     = "/usr/bin/htop"
                                  , mixer   = "/usr/bin/alsamixer"
                                  , notify  = "/usr/bin/notify-send"
                                  }
