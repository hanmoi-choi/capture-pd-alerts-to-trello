{-# LANGUAGE OverloadedStrings #-}

module Lib
        ( runApp
        )
where
import           Data.Foldable                  ( traverse_ )
import           OrchestrateApp
import           EffectInterpreters
import           Polysemy
import           Polysemy.Output
import           Polysemy.Error

runApp :: IO ()
runApp = do
        (logs, result) <-
                runM
                        $ ( runOutputList
                          . runError
                          . performFetchPagerDuty
                          . performFetchTrello
                          . performUpdateTrello
                          . decodeInputData
                          . readRawAppConfig
                          . readSystemEnv
                          )
                                  orchestrateApp
        traverse_ print logs
        case result of
                (Left  appError   ) -> putStrLn $ "Failed with: " <> show appError
                (Right finalResult) -> putStrLn $ "Completed with " <> show finalResult
