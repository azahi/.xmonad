-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Custom.Projects
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-- Custom target for projects.
--
------------------------------------------------------------------------

module XMonad.Custom.Projects
    ( projects
    ) where

import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.SpawnOn
import qualified XMonad.Custom.Misc             as CM

projects :: [Project]
projects =
    [ Project { projectName      = "Template"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }

    , Project { projectName      = "Emacs"
              , projectDirectory = "~/"
              , projectStartHook = Just $ spawnOn "Emacs" "/usr/bin/emacsclient"
              }

    , Project { projectName      = "WWW"
              , projectDirectory = "~/"
              , projectStartHook = Just $ spawnOn "WWW" (CM.browser CM.customApplications)
              }
    ]
