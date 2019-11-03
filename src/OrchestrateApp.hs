module OrchestrateApp where

import           Data.Text                      ( Text, pack )
import           Data.Foldable                  ( traverse_ )
import           Polysemy
import           Polysemy.Output
import           Polysemy.Error
import           Effects
import           Types.PagerDuty
import           Types.App
import           OrchestrateAppHelper

orchestrateApp :: Members
                   '[ReadSystemEnv
                    , ReadRawAppConfig
                    , DecodeInputData
                    , FetchPagerDutyRepo
                    , FetchTrelloRepo
                    , UpdateTrelloRepo
                    , Output Text
                    , Error AppError]
                   r
        => Sem r ()
orchestrateApp = do
        -- System Configuration
        systemEnv             <- performSyetemEnvRead
        rawAppConfig          <- performRawAppConfigRead
        appConfig             <- performDecode rawAppConfig

        -- Fetch PagerDuty Data
        Schedules schedules   <- performFetchPagerDutySchedules systemEnv appConfig
        Teams teams   <- performFetchPagerDutyTeams systemEnv appConfig
        schId                 <- fromEither $ extrackScheduleId schedules appConfig
        teamId                 <- fromEither $ extrackTeamId teams appConfig

        output $ (pack . show) schedules
        OnCalls oc            <- performFetchPagerDutyOnCalls systemEnv schId
        onCallUserId          <- fromEither $ extrackOnCallUserId oc
        output $ (pack . show) onCallUserId
        incidents             <- performFetchPagerDutyIncidents systemEnv teamId

        output $ (pack . show) incidents
        nonEmptyIncidents <- fromEither $ checkPagerDutyIncident incidents
        onCallUser            <- performFetchPagerDutyUser systemEnv onCallUserId

        -- Fetch Trello Data
        bs                    <- performFetchTrelloBoards systemEnv
        teamBoard             <- fromEither $ selectBoard bs appConfig
        members               <- performFetchTrelloMembers systemEnv teamBoard
        onCallMemberForTrello <- fromEither
                $ extrackOnCallMemberForTrello appConfig members onCallUser
        cards          <- performFetchTrelloCards systemEnv teamBoard
        lists          <- performFetchTrelloLists systemEnv teamBoard
        triageList     <- fromEither $ extrackTriageList appConfig lists
        labels         <- performFetchTrelloLabels systemEnv teamBoard
        selectedLabels <- fromEither $ selectLabelsForNewCard appConfig labels

        -- Create new Card
        newIncidents <- fromEither $ findNewIncidentNotInTrello nonEmptyIncidents cards
        let newTrelloCards = genNewTrelloCards newIncidents onCallMemberForTrello selectedLabels triageList
        _ <- traverse_ (createNewTrelloCard systemEnv) newTrelloCards
        pure ()
