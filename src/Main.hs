{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((>=>))
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitM, (.|), awaitForever, runConduit, yield)
import Data.Conduit.Binary (sourceIOHandle, sinkIOHandle, sourceLbs)
import Data.CSV.Conduit
    ( MapRow
    , intoCSV
    , runResourceT
    )
import Data.Text (Text)
import CommandLineOptions
    ( csvSettings
    , inputHandle
    , outputHandle
    , parseCommandLineOptions
    )

csvToJson :: Monad m => ConduitM (MapRow Text) ByteString m ()
csvToJson = awaitForever $ sourceLbs . encode >=> const (yield "\n")

main :: IO ()
main = do
    options <- parseCommandLineOptions
    runResourceT $ runConduit $
        sourceIOHandle (inputHandle options) .|
        intoCSV (csvSettings options) .|
        csvToJson .|
        sinkIOHandle (outputHandle options)
