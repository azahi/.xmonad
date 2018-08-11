{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Custom.Layout
-- Copyright   :  (c) azahi 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  azahi@teknik.io
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Custom.Layout
    ( layoutHook'
    , CustomTransformers (..)
    ) where

import           XMonad
import           XMonad.Custom.Theme
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Accordion
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.Hidden
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation

applySpacing :: l a -> ModifiedLayout Spacing l a
applySpacing = spacing gapBase

applyGaps :: l a -> ModifiedLayout Gaps l a
applyGaps = gaps [ (U, gapBase)
                 , (D, gapBase)
                 , (R, gapBase)
                 , (L, gapBase)
                 ]

data CustomTransformers = GAPS
                        deriving (Read, Show, Eq, Typeable)

instance Transformer CustomTransformers Window where
    transform GAPS x k = k (avoidStruts $ applyGaps $ applySpacing x) (const x)

layoutHook' = fullscreenFloat
            $ lessBorders OnlyFloat
            $ mkToggle (single NBFULL)
            $ avoidStruts
            $ mkToggle (single GAPS)
            $ mkToggle (single REFLECTX)
            $ mkToggle (single REFLECTY)
            $ windowNavigation
            $ addTabs shrinkText tabTheme
            $ hiddenWindows
            $ subLayout [] (Simplest ||| Accordion)
              emptyBSP
