{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.Trello where

import           Control.Lens.TH
import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                )
import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , withObject
                                                , (.:)
                                                )
import           Data.Text                      ( Text )

type Labels = [Label]
data Label = Label { _labelId :: Text
                   , _labelName    :: Text
                   } deriving (Show)
makeLenses ''Label

instance FromJSON Label where
        parseJSON = withObject "Label" $ \v -> Label <$> v .: "id" <*> v .: "name"

type Lists = [List]
data List = List { _listId :: Text
                 , _listName :: Text
                 } deriving (Show)

makeLenses ''List

instance FromJSON List where
        parseJSON = withObject "List" $ \v -> List <$> v .: "id" <*> v .: "name"

type Members = [Member]
data Member = Member { _memberId       :: Text
                     , _memberUserName :: Text
                     } deriving (Show)

makeLenses ''Member

instance FromJSON Member where
        parseJSON = withObject "Member" $ \v -> Member <$> v .: "id" <*> v .: "username"

type Cards = [Card]
data Card = Card { _cardId   :: Text
                 , _cardName :: Text
                 } deriving (Show)
makeLenses ''Card

instance FromJSON Card where
        parseJSON = withObject "Card" $ \v -> Card <$> v .: "id" <*> v .: "name"


type Boards = [Board]
data Board =
  Board
    { _boardId :: Text
    , _boardName :: Text
    , _boardUrl :: Text
    }
  deriving (Show, Eq)

makeLenses ''Board

instance FromJSON Board where
        parseJSON = withObject "Board" $ \v ->
                    Board <$> v .: "id" <*> v .: "name" <*> v .: "url"

data NewCard =
  NewCard
    { _newCardName       :: Text
    , _newCardDecription :: Text
    , _newCardAssigneeId :: Text
    , _newCardLabelIds   :: Text
    , _newCardListId     :: Text
    }
  deriving (Show)

makeLenses ''NewCard
