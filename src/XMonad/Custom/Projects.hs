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
