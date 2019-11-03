{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.PagerDuty where

import           Control.Lens.TH
import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                )
import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , withObject
                                                , withArray
                                                , (.:)
                                                )
import           Data.Text                      ( Text )

newtype UserRef = UserRef Text  deriving (Show, Eq)

instance FromJSON UserRef where
        parseJSON = withObject "UserRef" $ \v -> UserRef <$> v .: "id"

-- makeLenses ''UserRef
data OnCall = OnCall { _escalationLevel :: !Integer
                     , _startDate       :: !Text
                     , _endDate         :: !Text
                     , _userRef         :: UserRef
                     }  deriving (Show, Eq)

instance FromJSON OnCall where
        parseJSON =
                withObject "OnCall"
                        $ \v ->
                                  OnCall
                                          <$> v
                                          .:  "escalation_level"
                                          <*> v
                                          .:  "start"
                                          <*> v
                                          .:  "end"
                                          <*> v
                                          .:  "user"

makeLenses ''OnCall

newtype OnCalls = OnCalls [OnCall] deriving (Show, Eq)

instance FromJSON OnCalls where
        parseJSON = withObject "OnCalls" $ \v -> OnCalls <$> v .: "oncalls"

data User = User { _userId    :: !Text
                 , _userName  :: !Text
                 , _userEmail :: !Text
                 } deriving (Show)

makeLenses ''User

instance FromJSON User where
        parseJSON = withObject "User" $ \v -> do
                user  <- v .: "user"
                ui    <- user .: "id"
                name  <- user .: "name"
                email <- user .: "email"
                pure $ User ui name email

newtype Incidents =
  Incidents {incidents :: [Incident] } deriving (Show, Eq)

instance FromJSON Incidents where
        parseJSON = withObject "Incidents" $ \v -> Incidents <$> v .: "incidents"

data Incident = Incident { _incidentId      :: Text
                         , _incidentSummary :: Text
                         , _selfUrl         :: Text
                         } deriving (Show, Eq)

makeLenses ''Incident

instance FromJSON Incident where
        parseJSON = withObject "Incident"
                $ \v -> Incident <$> v .: "id" <*> v .: "summary" <*> v .: "self"

newtype Schedules = Schedules [Schedule] deriving (Show)

instance FromJSON Schedules where
        parseJSON = withObject "Schedules" $ \v -> Schedules <$> v .: "schedules"

data Schedule = Schedule { _scheduleId   :: !Text
                         , _scheduleName :: !Text
                         } deriving (Show)

instance FromJSON Schedule where
        parseJSON = withObject "Schedule" $ \v -> Schedule <$> v .: "id" <*> v .: "name"

makeLenses ''Schedule

newtype Teams = Teams [Team] deriving (Show)

instance FromJSON Teams where
        parseJSON = withObject "Teams" $ \v -> Teams <$> v .: "teams"

data Team = Team { _teamId   :: Text
                 , _teamName :: Text
                 } deriving (Show)

instance FromJSON Team where
        parseJSON = withObject "Team" $ \v -> Team <$> v .: "id" <*> v .: "name"

makeLenses ''Team
