{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web

A web-based interface to the 'JobScheduler'.
-}
module Web (app) where


import           Preamble

import qualified Data.ByteString.Lazy as BL
import qualified JobParser
import qualified JobScheduler
import qualified Web.Scotty           as Scotty


_200, _400, _422 :: Status
_200 = Status 202 "Job enqueued"
_400 = Status 400 "Malformed job spec"
_422 = Status 422 "Missing required form data"


-- | The web application. It is parameterized by 'JobQueue', which must be
-- separately initialized.
app :: JobScheduler.JobQueue -> Scotty.ScottyM ()
app jobQueue = do
    Scotty.get "/" $
        Scotty.text "hello"

    Scotty.post "/upload" $ do
        mJobSpec <- withUploadedFile JobParser.parse
        mJobSpec `or400` \jobSpec -> do
            JobScheduler.enqueue jobSpec jobQueue
            Scotty.status _200


-- Helper function to run a function on the content of an uploaded file.
withUploadedFile :: (BL.ByteString -> Maybe a) -> Scotty.ActionM (Maybe a)
withUploadedFile f = do
    files <- Scotty.files
    case files of
        [] -> do
            Scotty.status _422
            return Nothing
        ((_, fileInfo):_) ->
            return . f $ fileContent fileInfo


-- Apply 'f' to the given 'Maybe' value. If that's not possible, fail with
-- an HTTP 400 status message.
or400 :: Maybe a -> (a -> Scotty.ActionM ()) -> Scotty.ActionM ()
or400 m f = maybe (Scotty.status _400) f m
