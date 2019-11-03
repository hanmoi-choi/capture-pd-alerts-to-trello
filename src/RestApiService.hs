{-# LANGUAGE OverloadedStrings #-}

module RestApiService where

import           Control.Lens                   ( view )
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString               as BS
import           Data.Text                      ( unpack
                                                , append
                                                )
import qualified Network.HTTP.Conduit          as HC
import qualified Network.HTTP.Types            as HT
import           Network.HTTP.Client            ( Request )

import           Types.App

post :: Request -> IO ()
post initReq = do
        manager <- HC.newManager HC.tlsManagerSettings
        let request = initReq { HC.method = "POST" }
        _ <- HC.httpLbs request manager
        return ()

pagerDutyRequest :: SystemEnv -> ResourseName -> QueryStrings -> Request
pagerDutyRequest systemEnv (ResourseName rn) qs =
        let
                url     = append (view pagerDutyAPIUrl systemEnv) rn
                headers = pagerDutyRequestHeaders systemEnv
                initReq = (HC.parseRequest_ $ unpack url) { HC.method         = "GET"
                                                          , HC.requestHeaders = headers
                                                          }
        in
                HC.setQueryString qs initReq

pagerDutyRequestHeaders :: SystemEnv -> HT.RequestHeaders
pagerDutyRequestHeaders systemEnv =
        [ ("Authorization", encodeUtf8 $ append "Token token=" $ view pagerDutyAPIToken systemEnv)
        , ("Accept"       , "application/vnd.pagerduty+json;version=2")
        ]

getAndExtractBody :: Request -> IO BS.ByteString
getAndExtractBody request = do
        manager <- HC.newManager HC.tlsManagerSettings
        lazyRsp <- HC.httpLbs request manager
        let rsp = BSL.toStrict <$> lazyRsp
        return $ HC.responseBody rsp

trelloRequest :: SystemEnv -> ResourseName -> QueryStrings -> Request
trelloRequest systemEnv (ResourseName rn) qs =
        let url     = append (view trelloAPIUrl systemEnv) rn
            initReq = (HC.parseRequest_ $ unpack url)
        in  HC.setQueryString (qs ++ trelloAuthQueryString systemEnv) initReq

trelloAuthQueryString :: SystemEnv -> QueryStrings
trelloAuthQueryString systemEnv =
        [ ("key"  , Just $ encodeUtf8 (view trelloAPIKey systemEnv))
        , ("token", Just $ encodeUtf8 (view trelloAPIToken systemEnv))
        ]
