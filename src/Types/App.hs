{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.App where

import           Control.Lens.TH
import           Data.Text                      ( Text )
import qualified Data.ByteString               as BS

data AppError =
    ParseFailure
  | EmptyOnCallList
  | PagerDutyOnCallNotFound
  | PagerDutyScheduleNotFound
  | PagerDutyTeamNotFound
  | PagerDutyEmptyIncidents
  | TrelloBoardNotFound
  | TrelloTriageColumnNotFound
  | TrelloMemberNotFound
  | TrelloLabelsNotFound
  | TrelloNoNewCards
  deriving Show

data SystemEnv = SystemEnv { _trelloAPIUrl :: Text
                           , _trelloAPIKey :: Text
                           , _trelloAPIToken :: Text
                           , _pagerDutyAPIUrl :: Text
                           , _pagerDutyAPIToken :: Text
                           } deriving (Show, Eq)

makeLenses ''SystemEnv

type QueryStrings = [(BS.ByteString, Maybe BS.ByteString)]
newtype ResourseName = ResourseName Text deriving (Show, Eq)
