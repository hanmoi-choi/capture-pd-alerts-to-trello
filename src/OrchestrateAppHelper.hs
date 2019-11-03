module OrchestrateAppHelper where

import           Data.Text                      ( Text
                                                , intercalate
                                                )
import Data.List (find)
import           Control.Lens                   ( view )
import qualified Types.PagerDuty               as PD
import qualified Types.AppConfig               as AC
import qualified Types.Trello                  as T
import           Types.App

findNewIncidentNotInTrello :: PD.Incidents -> T.Cards -> Either AppError PD.Incidents
findNewIncidentNotInTrello (PD.Incidents incidents) cards =
    let
       cardNames = map (view T.cardName) cards
       selectedIncidents = filter (\i -> (view PD.incidentSummary i) `notElem` cardNames ) incidents
    in case selectedIncidents of
      [] -> Left TrelloNoNewCards
      xi -> Right (PD.Incidents xi)

selectLabelsForNewCard :: AC.AppConfig -> T.Labels -> Either AppError T.Labels
selectLabelsForNewCard appConfig labels =
    let
       labelsFromConfig = view (AC.trelloConfig . AC.labelsForNewCard) appConfig
       labelsTextFromConfig = map (\(AC.NewCardLabel t) -> t) labelsFromConfig
       selectedLabels = filter (\l -> (view T.labelName l) `elem` labelsTextFromConfig) labels
    in case selectedLabels of
      [] -> Left TrelloLabelsNotFound
      xl -> Right xl

extrackOnCallMemberForTrello :: AC.AppConfig -> T.Members -> PD.User -> Either AppError T.Member
extrackOnCallMemberForTrello appConfig tMembers user =
    let
        pdUserEmail = view PD.userEmail user
        trelloMemberDetails = view (AC.trelloConfig . AC.memberDetails) appConfig
        selectedTrelloMember = do
          selectedMemberDetail <- find (\md -> view AC.oktaEmail md == pdUserEmail) trelloMemberDetails
          find (\m -> view T.memberUserName m == view AC.trelloId selectedMemberDetail) tMembers
    in
       case selectedTrelloMember of
         Just m -> Right m
         Nothing -> Left TrelloMemberNotFound

checkPagerDutyIncident :: PD.Incidents -> Either AppError PD.Incidents
checkPagerDutyIncident ix@(PD.Incidents incidents) =
    case incidents of
      [] -> Left PagerDutyEmptyIncidents
      _ -> Right ix

extrackTriageList :: AC.AppConfig -> T.Lists -> Either AppError T.List
extrackTriageList appConfig lists =
    let
        AC.ColumnForTriage colName = view (AC.trelloConfig . AC.columnForTriage) appConfig
        filteredList = find (\l -> view T.listName l ==  colName) lists
    in
      case filteredList of
        Just v -> Right v
        Nothing -> Left TrelloTriageColumnNotFound

extrackOnCallUserId :: [PD.OnCall] -> Either AppError Text
extrackOnCallUserId onCalls =
    let
        fstEscalationLevel = 1
        fstLevelOnCall = find (\oc -> view PD.escalationLevel oc == fstEscalationLevel) onCalls
    in case fstLevelOnCall of
         Nothing   -> Left PagerDutyOnCallNotFound
         Just cx ->
             let (PD.UserRef uId) = view PD.userRef cx
             in Right uId

extrackScheduleId :: [PD.Schedule] -> AC.AppConfig -> Either AppError Text
extrackScheduleId schedules appConfig =
    let AC.ScheduleName scheduleName = view (AC.pagerDutyConfig . AC.scheduleName) appConfig
        filteredSchedule = find (\s -> view PD.scheduleName s == scheduleName) schedules
    in case filteredSchedule of
         Just s -> Right $ view PD.scheduleId s
         Nothing   -> Left PagerDutyScheduleNotFound

extrackTeamId :: [PD.Team] -> AC.AppConfig -> Either AppError Text
extrackTeamId schedules appConfig =
    let AC.TeamName teamName = view (AC.pagerDutyConfig . AC.teamName) appConfig
        filteredTeam = find (\s -> view PD.teamName s == teamName) schedules
    in case filteredTeam of
         Just s -> Right $ view PD.teamId s
         Nothing   -> Left PagerDutyTeamNotFound

selectBoard :: T.Boards -> AC.AppConfig -> Either AppError T.Board
selectBoard boards appConfig =
        let AC.BoardName bn = view (AC.trelloConfig . AC.boardName) appConfig
            fbs             = find (\b -> view T.boardName b == bn) boards
        in  case fbs of
                    Just b -> Right b
                    Nothing   -> Left TrelloBoardNotFound

genNewTrelloCards :: PD.Incidents -> T.Member -> T.Labels -> T.List -> [T.NewCard]
genNewTrelloCards (PD.Incidents newIncidents) onCallMemberForTrello labels triageList =
        let trelloMemberId = view T.memberId onCallMemberForTrello
            listId         = view T.listId triageList
            labelIds       = intercalate "," $ map (view T.labelId) labels
            genNewCard name desc = T.NewCard name desc trelloMemberId labelIds listId
        in  map
                    (\i ->
                            let summary = view PD.incidentSummary i
                                url     = view PD.selfUrl i
                            in  genNewCard summary url
                    )
                    newIncidents
