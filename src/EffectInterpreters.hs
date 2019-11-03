module EffectInterpreters where

import           Control.Lens                   ( view )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Bifunctor                 ( first )
import           Data.Yaml                      ( decodeEither' )
import qualified Polysemy as P
import           Polysemy.Output
import           Polysemy.Error
import           System.Environment             ( getEnv )
import           Data.Text                      ( Text
                                                , intercalate
                                                , append
                                                , pack
                                                )
import           RestApiService
import           Types.App
import           Types.AppConfig
import qualified Types.Trello                  as T
import           Effects

readSystemEnv :: P.Member (P.Embed IO) r => P.Sem (ReadSystemEnv ': r) a -> P.Sem r a
readSystemEnv = P.interpret $ \PerformSyetemEnvRead -> P.embed $ do
        tApiURL   <- pack <$> getEnv "TRELLO_API_URL"
        tApiKey   <- pack <$> getEnv "TRELLO_API_KEY"
        tApiToken <- pack . filter (/= '\n') <$> getEnv "TRELLO_API_TOKEN"
        pApiUrl   <- pack <$> getEnv "PAGERDUTY_API_URL"
        pApiToken <- pack . filter (/= '\n') <$> getEnv "PAGERDUTY_API_TOKEN"
        return $ SystemEnv tApiURL tApiKey tApiToken pApiUrl pApiToken

readRawAppConfig :: P.Member (P.Embed IO) r => P.Sem (ReadRawAppConfig ': r) a -> P.Sem r a
readRawAppConfig = P.interpret $ \PerformRawAppConfigRead ->
        P.embed $ RawAppConfig . pack <$> readFile "config/the-castle.yml"

decodeInputData :: P.Member (Error AppError) r => P.Sem (DecodeInputData ': r) a -> P.Sem r a
decodeInputData = P.interpret $ \(PerformDecode (RawAppConfig rawText)) ->
        fromEither $ first (const ParseFailure) $ decodeEither' $ encodeUtf8 rawText

performFetchPagerDuty
        :: P.Members '[P.Embed IO, Output Text, Error AppError] r
        => P.Sem (FetchPagerDutyRepo ': r) a
        -> P.Sem r a
performFetchPagerDuty = P.interpret $ \case
        PerformFetchPagerDutySchedules systemEnv appConfig ->
                let ScheduleName tn = view (pagerDutyConfig . scheduleName) appConfig
                    queryString = [("query", Just $ encodeUtf8 tn)]
                    request     = pagerDutyRequest systemEnv (ResourseName "schedules") queryString
                in  do
                            body <- P.embed $ getAndExtractBody request
                            fromEither $ first (const ParseFailure) $ decodeEither' body
        PerformFetchPagerDutyTeams systemEnv appConfig ->
                let TeamName tn = view (pagerDutyConfig . teamName) appConfig
                    queryString = [("query", Just $ encodeUtf8 tn)]
                    request     = pagerDutyRequest systemEnv (ResourseName "teams") queryString
                in  do
                            body <- P.embed $ getAndExtractBody request
                            fromEither $ first (const ParseFailure) $ decodeEither' body

        PerformFetchPagerDutyOnCalls systemEnv schId ->
                let queryString =
                                    [ ("time_zone"     , Just $ encodeUtf8 "Australia/Melbourne")
                                    , ("schedule_ids[]", Just $ encodeUtf8 schId)
                                    ]
                    request = pagerDutyRequest systemEnv (ResourseName "oncalls") queryString
                in  do
                            body <- P.embed $ getAndExtractBody request
                            fromEither $ first (const ParseFailure) $ decodeEither' body

        PerformFetchPagerDutyIncidents systemEnv schId ->
                let queryString =
                                    [ ("time_zone" , Just $ encodeUtf8 "Australia/Melbourne")
                                    , ("statuses[]", Just $ encodeUtf8 "triggered")
                                    , ("statuses[]", Just $ encodeUtf8 "acknowledged")
                                    , ("team_ids[]", Just $ encodeUtf8 schId)
                                    ]
                    request = pagerDutyRequest systemEnv (ResourseName "incidents") queryString
                in  do
                            body <- P.embed $ getAndExtractBody request
                            fromEither $ first (const ParseFailure) $ decodeEither' body
        PerformFetchPagerDutyUser systemEnv uId ->
                let
                        queryString = []
                        request     = pagerDutyRequest systemEnv
                                                       (ResourseName $ append "users/" uId)
                                                       queryString
                in
                        do
                                body <- P.embed $ getAndExtractBody request
                                fromEither $ first (const ParseFailure) $ decodeEither' body


performFetchTrello
        :: P.Members '[P.Embed IO, Output Text, Error AppError] r
        => P.Sem (FetchTrelloRepo ': r) a
        -> P.Sem r a
performFetchTrello = P.interpret $ \case
        PerformFetchTrelloBoards systemEnv ->
                let queryString = [("fields", Just $ encodeUtf8 "name,url")]
                    request = trelloRequest systemEnv
                                                (ResourseName "members/me/boards")
                                                queryString
                in
                        do
                                body <- P.embed $ getAndExtractBody request
                                fromEither $ first (const ParseFailure) $ decodeEither' body
        PerformFetchTrelloCards systemEnv board ->
                let queryString = ("fields", Just $ encodeUtf8 "name")
                            : trelloAuthQueryString systemEnv
                    bId     = view T.boardId board
                    rn      = intercalate "/" ["boards", bId, "cards"]
                    request = trelloRequest systemEnv (ResourseName rn) queryString
                in  do
                            body <- P.embed $ getAndExtractBody request
                            fromEither $ first (const ParseFailure) $ decodeEither' body
        PerformFetchTrelloLists systemEnv board ->
                let queryString = [ ("cards" , Just $ encodeUtf8 "none")
                                  , ("filter", Just $ encodeUtf8 "open")
                                  , ("fields", Just $ encodeUtf8 "name")
                                  ]
                    bId     = view T.boardId board
                    rn      = intercalate "/" ["boards", bId, "lists"]
                    request = trelloRequest systemEnv (ResourseName rn) queryString
                in  do
                            body <- P.embed $ getAndExtractBody request
                            fromEither $ first (const ParseFailure) $ decodeEither' body
        PerformFetchTrelloLabels systemEnv board ->
                let queryString = [("fields", Just $ encodeUtf8 "name")]
                    bId     = view T.boardId board
                    rn      = intercalate "/" ["boards", bId, "labels"]
                    request = trelloRequest systemEnv (ResourseName rn) queryString
                in  do
                            body <- P.embed $ getAndExtractBody request
                            fromEither $ first (const ParseFailure) $ decodeEither' body
        PerformFetchTrelloMembers systemEnv board ->
                let queryString = [("fields", Just $ encodeUtf8 "all")]
                    bId     = view T.boardId board
                    rn      = intercalate "/" ["boards", bId, "members"]
                    request = trelloRequest systemEnv (ResourseName rn) queryString
                in  do
                            body <- P.embed $ getAndExtractBody request
                            fromEither $ first (const ParseFailure) $ decodeEither' body

performUpdateTrello
        :: P.Members '[P.Embed IO, Output Text, Error AppError] r
        => P.Sem (UpdateTrelloRepo ': r) a
        -> P.Sem r a
performUpdateTrello = P.interpret $ \case
        CreateNewTrelloCard systemEnv newCard ->
                let queryString = [ ("idMembers" , Just $ encodeUtf8 $ view T.newCardAssigneeId newCard)
                                  , ("idLabels", Just $ encodeUtf8 $ view T.newCardLabelIds newCard)
                                  , ("idList", Just $ encodeUtf8 $ view T.newCardListId newCard)
                                  , ("name", Just $ encodeUtf8 $ view T.newCardName newCard)
                                  , ("desc", Just $ encodeUtf8 $ view T.newCardDecription newCard)
                                  , ("keepFromSource", Just $ encodeUtf8 "all")
                                  ]
                    request = trelloRequest systemEnv (ResourseName "cards") queryString
                in P.embed $ post request
