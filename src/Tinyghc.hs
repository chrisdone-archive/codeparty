-- |

module Tinyghc where

import           Data.Aeson
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           System.Exit
import           System.IO.Temp
import           System.Process

data Compile
  = Compiled ExitCode
             Value
  | InvalidGhcJson String
  deriving (Show)

data Run = Run
  { runCode :: ExitCode
  , runOut :: Text
  , runErr :: Text
  } deriving (Show)

compile :: Text -> IO Compile
compile input = do
  withSystemTempDirectory
    "tinyghc"
    (\pwd -> do
       T.writeFile "main.hs" input
       (exitCode, out, err) <-
         readProcessWithExitCode
           "docker"
           (dockerCall pwd "ghc -fforce-recomp main.hs -Wall -fforce-recomp -ddump-json")
           ""
       case decode (LT.encodeUtf8 (LT.pack out)) of
         Nothing -> pure (InvalidGhcJson err)
         Just j -> pure (Compiled exitCode j))

run :: Text -> IO Run
run input = do
  withSystemTempDirectory
    "tinyghc"
    (\pwd -> do
       T.writeFile "main.hs" input
       (exitCode, out, err) <-
         readProcessWithExitCode
           "docker"
           (dockerCall
              pwd
              "ghc main.hs -fforce-recomp 1>/dev/null 2>/dev/null && ./x")
           ""
       pure (Run exitCode (T.pack out) (T.pack err)))

dockerCall :: String -> String -> [String]
dockerCall pwd cmd =
  [ "docker"
  , "run"
  , "--memory"
  , "10m"
  , "--memory-swap"
  , "10m"
  , "-v"
  , pwd <> ":/home/tinyghc/"
  , "--rm"
  , "-i"
  , "tinyghc"
  , "su"
  , "-"
  , "tinyghc"
  , "-c"
  , cmd
  ]
