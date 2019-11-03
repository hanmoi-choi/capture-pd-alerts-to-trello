{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.AppConfig where

import           GHC.Generics                   ( Generic )
import           Data.Yaml
import           Control.Lens.TH
import           Data.Text                      ( Text
                                                , splitOn
                                                )

newtype RawAppConfig = RawAppConfig Text deriving (Eq, Show)

newtype BoardName = BoardName Text deriving (Show, Generic, Eq)

newtype ColumnForTriage = ColumnForTriage Text deriving (Show, Generic, Eq)

newtype NewCardLabel = NewCardLabel Text deriving (Show, Generic, Eq)

data MemberDetail = MemberDetail { _trelloId :: Text
                                 , _oktaEmail :: Text
                                 } deriving (Show, Eq)

makeLenses ''MemberDetail

data TrelloConfig = TrelloConfig { _boardName :: BoardName
                                 , _columnForTriage :: ColumnForTriage
                                 , _memberDetails :: [MemberDetail]
                                 , _labelsForNewCard :: [NewCardLabel]
                                 } deriving (Eq, Show)

makeLenses ''TrelloConfig

newtype TeamName = TeamName Text deriving (Eq, Show, Generic)

newtype ScheduleName = ScheduleName Text deriving (Eq, Show, Generic)

data PagerDutyConfig = PagerDutyConfig { _teamName :: TeamName
                                       , _scheduleName :: ScheduleName
                                       } deriving (Eq, Show)

makeLenses ''PagerDutyConfig

data AppConfig = AppConfig { _trelloConfig :: TrelloConfig
                           , _pagerDutyConfig :: PagerDutyConfig
                           } deriving (Eq, Show)

makeLenses ''AppConfig


instance FromJSON BoardName where
        parseJSON = withText "BoardName" $ \t -> return $ BoardName t

instance FromJSON ColumnForTriage where
        parseJSON = withText "ColumnForTriage" $ \t -> return $ ColumnForTriage t

instance FromJSON NewCardLabel where
        parseJSON = withText "NewCardLabel" $ \t -> return $ NewCardLabel t

instance FromJSON MemberDetail where
        parseJSON = withText "MemberDetail"
                $ \t ->
                    let [tid, e] = splitOn "," t
                    in return $ MemberDetail tid e

instance FromJSON TeamName where
        parseJSON = withText "TeamName" $ \t -> return $ TeamName t

instance FromJSON ScheduleName where
        parseJSON = withText "ScheduleName" $ \t -> return $ ScheduleName t

instance FromJSON AppConfig where
        parseJSON = withObject "AppConfig" $ \v -> do
                trello <- v .: "trello"
                pd <- v .: "pagerduty"
                pure $ AppConfig trello pd

instance FromJSON PagerDutyConfig where
        parseJSON = withObject "PagerDutyConfig" $ \v -> do
                tn  <- v .: "team-name"
                sn  <- v .: "schedule-name"
                pure $ PagerDutyConfig tn sn

instance FromJSON TrelloConfig where
        parseJSON = withObject "TrelloConfig" $ \v -> do
                bn  <- v .: "board-name"
                cft <- v .: "column-for-triage"
                mds <- v .: "members"
                ls <- v .: "labels-for-new-card"
                pure $ TrelloConfig bn cft mds ls
