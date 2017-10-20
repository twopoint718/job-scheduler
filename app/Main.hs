module Main where

import qualified JobScheduler
import           Preamble
import qualified Web
import           Web.Scotty   (scotty)

main :: IO ()
main    =
    bracket (JobScheduler.init 4) (JobScheduler.shutdown 4) $ \jobQueue ->
        scotty 8000 (Web.app jobQueue)
