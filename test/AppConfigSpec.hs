{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module AppConfigSpec where

import           Data.Yaml
import           Test.Hspec
import           Text.RawString.QQ
import           Data.ByteString                ( ByteString )

import           Types.AppConfig


trelloConfigYAML :: ByteString
trelloConfigYAML = [r|---
# All information string values should be correct for functioning correctly
trello:
  board-name: The Castle
  column-for-triage: Triage
  members:
    - "danielchoi27,daniel.choi@rea-group.com"
    - "ayseasalioglu1,ayse.asalioglu@rea-group.com"

  labels-for-new-card:
    - unplanned
    - Custodianship

pagerduty:
  team-name: Group Technology - DS - Property Insights
  schedule-name: Group Technology - DS - Property Insights - Business Hours|]


spec :: Spec
spec = describe "FromJSON" $ do
        it "decodes the AppConfig" $ do
                let     result          = decodeEither trelloConfigYAML
                        expected        = Right $ AppConfig trello pagerDuty
                        trello          = TrelloConfig board column members labels
                        board           = (BoardName "The Castle")
                        column = (ColumnForTriage "Triage")
                        members =
                                [ MemberDetail "danielchoi27"   "daniel.choi@rea-group.com"
                                , MemberDetail "ayseasalioglu1" "ayse.asalioglu@rea-group.com"
                                ]
                        labels    = [NewCardLabel "unplanned", NewCardLabel "Custodianship"]
                        pagerDuty = PagerDutyConfig teamName scheduleName
                        scheduleName  = ScheduleName "Group Technology - DS - Property Insights - Business Hours"
                        teamName  = TeamName "Group Technology - DS - Property Insights"
                result `shouldBe` expected
