module XMonad.Custom.Workspaces
    ( workspaces'
    ) where

import           XMonad.Core

workspaces' :: [WorkspaceId]
workspaces' = map show [1 .. 9 :: Int]
