-- |

module Main where

import System.Exit
import System.Process

main :: IO ExitCode
main = do
  putStrLn "Building project ..."
  buildResult <-
    rawSystem "psc-package" ["build", "--", "-o", ".psc-package-work"]
  case buildResult of
    ExitFailure e -> exitWith (ExitFailure e)
    ExitSuccess -> do
      putStrLn ("Bundling to " ++ outfp ++ " ...")
      bundleResult <-
        rawSystem
          "purs"
          [ "bundle"
          , ".psc-package-work/**/*.js"
          , "-m"
          , "Main"
          , "--main"
          , "Main"
          , "-o"
          , outfp
          ]
      exitWith bundleResult
  where
    outfp = "static/index.js"
