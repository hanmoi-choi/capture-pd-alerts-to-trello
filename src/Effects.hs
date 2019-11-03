{-# LANGUAGE TemplateHaskell #-}

module Effects where

import qualified Polysemy as P (makeSem)

import           Data.Text
import           Types.App
import           Types.AppConfig
import           Types.PagerDuty
import           Types.Trello

data A = A | B | C

data ReadSystemEnv m a where
  PerformSyetemEnvRead ::ReadSystemEnv m SystemEnv

data ReadRawAppConfig m a where
  PerformRawAppConfigRead ::ReadRawAppConfig m RawAppConfig

data DecodeInputData m a where
  PerformDecode ::RawAppConfig -> DecodeInputData m AppConfig

data FetchPagerDutyRepo m a where
  PerformFetchPagerDutySchedules ::SystemEnv -> AppConfig -> FetchPagerDutyRepo m Schedules
  PerformFetchPagerDutyTeams ::SystemEnv -> AppConfig -> FetchPagerDutyRepo m Teams
  PerformFetchPagerDutyOnCalls ::SystemEnv -> Text -> FetchPagerDutyRepo m OnCalls
  PerformFetchPagerDutyIncidents ::SystemEnv -> Text -> FetchPagerDutyRepo m Incidents
  PerformFetchPagerDutyUser ::SystemEnv -> Text -> FetchPagerDutyRepo m User

data FetchTrelloRepo m a where
  PerformFetchTrelloBoards ::SystemEnv -> FetchTrelloRepo m Boards
  PerformFetchTrelloCards ::SystemEnv -> Board -> FetchTrelloRepo m Cards
  PerformFetchTrelloLists ::SystemEnv -> Board -> FetchTrelloRepo m Lists
  PerformFetchTrelloLabels ::SystemEnv -> Board -> FetchTrelloRepo m Labels
  PerformFetchTrelloMembers ::SystemEnv -> Board -> FetchTrelloRepo m Members

data UpdateTrelloRepo m a where
  CreateNewTrelloCard :: SystemEnv -> NewCard -> UpdateTrelloRepo m ()

P.makeSem ''ReadSystemEnv
P.makeSem ''ReadRawAppConfig
P.makeSem ''DecodeInputData
P.makeSem ''FetchPagerDutyRepo
P.makeSem ''FetchTrelloRepo
P.makeSem ''UpdateTrelloRepo
