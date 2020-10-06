{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Language where

import           BeatMapGenTypes

import           Control.Monad.Free.Church
import           Control.Monad.Free.TH

import           Control.Monad

{- ANALYSIS -}

data AnalysisLang next where
  ProcSection :: logger -> (logger -> next) -> AnalysisLang next
  MakeEvent :: section -> frame -> (event -> next) -> AnalysisLang next

instance Functor AnalysisLang where
  fmap f (ProcSection logger next) = ProcSection logger (f . next)
  fmap f (MakeEvent section frame next) = MakeEvent section frame (f . next)

makeFreeCon 'ProcSection
makeFreeCon 'MakeEvent

type AnalysisF f = F AnalysisLang f

{- LOGGER -}

data LoggerLang next where
  UpdateLog :: song -> event -> (generator -> next) -> LoggerLang next

  GetFrame :: generator -> (generator -> frame) -> LoggerLang frame
  GetSong  :: generator -> (generator -> song ) -> LoggerLang song

  GetFinalBeatMap :: (beatmap -> next) -> LoggerLang next

instance Functor LoggerLang where
  fmap f (UpdateLog song event next) = UpdateLog song event (f . next )
  fmap f (GetFrame generator frame ) = GetFrame  generator  (f . frame)
  fmap f (GetSong  generator song  ) = GetSong   generator  (f . song )
  fmap f (GetFinalBeatMap beatmap  ) = GetFinalBeatMap    (f . beatmap)

makeFreeCon 'UpdateLog
makeFreeCon 'GetFrame
makeFreeCon 'GetSong
makeFreeCon 'GetFinalBeatMap

type LoggerF   f = F LoggerLang   f

interpAnalysis :: AnalysisF f -> f
interpAnalysis = undefined

interpLogger :: LoggerF f -> f
interpLogger = undefined

runAnalysis = interpAnalysis . procSection

analyzeSection tracker = procSection <$> newLogger
  where splitSong = interpLogger (getSong  tracker)
        curFrame  = interpLogger (getFrame tracker)
        newEvent  = interpAnalysis $ makeEvent <$> (fst <$> splitSong) <*> curFrame
        newLogger = interpLogger $ updateLog <$> (snd <$> splitSong) <*> newEvent
