-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Custom.Workspaces
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Custom.Workspaces
    ( workspaces'
    ) where

import           XMonad.Actions.DynamicProjects
import           XMonad.Core
import           XMonad.Custom.Misc

workspaces' :: [WorkspaceId]
workspaces' = map show [1..9 :: Int]

projects :: [Project]
projects =
    [ Project { projectName      = "scratch"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }

    , Project { projectName      = "www"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }

    , Project { projectName      = "mail"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    ]
