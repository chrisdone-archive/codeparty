-- |

module Tinyghc where

import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Exit
import           System.IO.Temp
import           System.Process

compile :: Text -> IO (ExitCode, Text, Text)
compile input = do
  withSystemTempDirectory
    "tinyghc"
    (\pwd -> do
       T.writeFile "main.hs" input
       (exitCode, out, err) <-
         readProcessWithExitCode
           "docker"
           [ "docker"
           , "run"
           , "-v"
           , pwd <> ":/home/tinyghc/"
           , "--rm"
           , "-i"
           , "tinyghc"
           , "su"
           , "-"
           , "tinyghc"
           , "-c"
           , "ghc -dynamic x.hs -Wall -fforce-recomp"
           ]
           ""
       pure (exitCode, T.pack out, T.pack err))

run :: Text -> IO (ExitCode, Text, Text)
run input = do
  withSystemTempDirectory
    "tinyghc"
    (\pwd -> do
       T.writeFile "main.hs" input
       (exitCode, out, err) <-
         readProcessWithExitCode
           "docker"
           [ "docker"
           , "run"
           , "-v"
           , pwd <> ":/home/tinyghc/"
           , "--rm"
           , "-i"
           , "tinyghc"
           , "su"
           , "-"
           , "tinyghc"
           , "-c"
           , "ghc -dynamic x.hs -fforce-recomp 1>/dev/null 2>/dev/null && ./x"
           ]
           ""
       pure (exitCode, T.pack out, T.pack err))
