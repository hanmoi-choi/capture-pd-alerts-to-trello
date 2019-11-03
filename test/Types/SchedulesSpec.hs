{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Types.SchedulesSpec where

import           Test.Hspec
import           Data.Yaml
import           Data.ByteString                ( ByteString )
import           Types.PagerDuty
import           Text.RawString.QQ

onCallsJson :: ByteString
onCallsJson = [r|
{
  "oncalls": [
    {
      "escalation_policy": {
        "id": "PVWESAR",
        "type": "escalation_policy_reference",
        "summary": "Group Technology - DS - Property Insights - Default",
        "self": "https://api.pagerduty.com/escalation_policies/PVWESAR",
        "html_url": "https://rea-group.pagerduty.com/escalation_policies/PVWESAR"
      },
      "escalation_level": 2,
      "schedule": {
        "id": "PYCJDLU",
        "type": "schedule_reference",
        "summary": "Group Technology - DS - Property Insights - Business Hours",
        "self": "https://api.pagerduty.com/schedules/PYCJDLU",
        "html_url": "https://rea-group.pagerduty.com/schedules/PYCJDLU"
      },
      "user": {
        "id": "PS5EU0A",
        "type": "user_reference",
        "summary": "Daniel Choi",
        "self": "https://api.pagerduty.com/users/PS5EU0A",
        "html_url": "https://rea-group.pagerduty.com/users/PS5EU0A"
      },
      "start": "2019-08-21T09:00:00+10:00",
      "end": "2019-08-21T17:00:00+10:00"
    },
    {
      "escalation_policy": {
        "id": "PVWESAR",
        "type": "escalation_policy_reference",
        "summary": "Group Technology - DS - Property Insights - Default",
        "self": "https://api.pagerduty.com/escalation_policies/PVWESAR",
        "html_url": "https://rea-group.pagerduty.com/escalation_policies/PVWESAR"
      },
      "escalation_level": 1,
      "schedule": {
        "id": "PYCJDLU",
        "type": "schedule_reference",
        "summary": "Group Technology - DS - Property Insights - Business Hours",
        "self": "https://api.pagerduty.com/schedules/PYCJDLU",
        "html_url": "https://rea-group.pagerduty.com/schedules/PYCJDLU"
      },
      "user": {
        "id": "PS5EU0A",
        "type": "user_reference",
        "summary": "Daniel Choi",
        "self": "https://api.pagerduty.com/users/PS5EU0A",
        "html_url": "https://rea-group.pagerduty.com/users/PS5EU0A"
      },
      "start": "2019-08-21T09:00:00+10:00",
      "end": "2019-08-21T17:00:00+10:00"
    }
  ],
  "limit": 25,
  "offset": 0,
  "more": false,
  "total": null
}
|]


spec :: Spec
spec = describe "FromJSON" $ do
        it "decodes the OnCallJson" $ do
                let     result    = decodeEither onCallsJson
                        expected  = Right $ OnCalls [sndOnCall, fstOnCall]
                        fstOnCall = OnCall 1
                                           "2019-08-21T09:00:00+10:00"
                                           "2019-08-21T17:00:00+10:00"
                                           (UserRef "PS5EU0A")
                        sndOnCall = OnCall 2
                                           "2019-08-21T09:00:00+10:00"
                                           "2019-08-21T17:00:00+10:00"
                                           (UserRef "PS5EU0A")
                result `shouldBe` expected
