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
------------------------------------------------------------------------

module XMonad.Custom.Workspaces
    ( workspaces'
    ) where

import           XMonad.Core

workspaces' :: [WorkspaceId]
workspaces' = map show [1..9 :: Int]
