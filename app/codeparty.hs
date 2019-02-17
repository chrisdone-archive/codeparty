{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text.IO as T
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import           Network.HTTP.Types.Status as Http
import qualified Options.Applicative.Simple as Opts
import           System.Directory
import           System.FSNotify as FSNotify

main :: IO ()
main = do
  ((), runCmd) <-
    Opts.simpleOptions
      "ver"
      "header"
      "Codeparty CLI"
      (pure ())
      (do Opts.addCommand
            "watch"
            "Watch changes to FILE and send them to codeparty"
            id
            (watch <$>
             Opts.strOption
               (Opts.long "uri" <> Opts.value "https://codeparty.in" <>
                Opts.help "Web service URI") <*>
             Opts.strOption
               (Opts.long "room" <> Opts.help "Room to broadcast to") <*>
             Opts.strOption
               (Opts.long "token" <> Opts.help "Your user token" <>
                Opts.metavar "TOKEN") <*>
             Opts.strArgument
               (Opts.help "File to watch" <> Opts.metavar "FILENAME") <*>
             Opts.strOption
               (Opts.long "title" <> Opts.help "Title of your editor")))
  runCmd

watch :: String -> String -> String -> String -> FilePath -> IO ()
watch uri room token file title = do
  fp <- canonicalizePath file
  initialRequest <- Http.parseRequest (uri ++ "/" ++ room)
  httpManager <- Http.newManager Http.tlsManagerSettings
  FSNotify.withManager
    (\fsManager -> do
       _ <-
         watchDir
           fsManager
           "."
           (\case
              Modified fp' _ _ -> fp' == fp
              Added fp' _ _ -> fp' == fp
              _ -> False)
           (\ev -> do
              body <- T.readFile fp
              let request =
                    initialRequest
                      { Http.method = "POST"
                      , Http.requestBody =
                          Http.RequestBodyLBS $
                          Json.encode
                            (Json.object
                               [("input" Json..= body), ("title" Json..= title)])
                      , Http.requestHeaders =
                          (Http.requestHeaders initialRequest) <>
                          [("Authorization", S8.pack token)]
                      }
              response <- Http.httpLbs request httpManager
              if Http.statusCode (Http.responseStatus response) == 200
                then putStrLn (show (eventTime ev) ++ ": Submitted.")
                else putStrLn
                       (show (eventTime ev) ++
                        ": FAILED TO SUBMIT: " ++
                        show (Http.responseBody response)))
       forever $ threadDelay 1000000)
