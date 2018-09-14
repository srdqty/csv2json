{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((>=>))
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitM, (.|), awaitForever, runConduit, yield)
import Data.Conduit.Binary (sourceHandle, sinkHandle, sourceLbs)
import Data.CSV.Conduit
    ( MapRow
    , csvQuoteChar
    , defCSVSettings
    , intoCSV
    , runResourceT
    )
import Data.Text (Text)
import System.IO
    ( BufferMode (BlockBuffering)
    , hSetBuffering
    , stdin
    , stdout
    )

-- TODO: Add command line options

bufSize :: Int
bufSize = 2*1024*1024

csvToJson :: Monad m => ConduitM (MapRow Text) ByteString m ()
csvToJson = awaitForever $ sourceLbs . encode >=> const (yield "\n")

main :: IO ()
main = do
    let csvSettings = defCSVSettings { csvQuoteChar = Nothing }
    hSetBuffering stdout (BlockBuffering (Just (bufSize*2)))
    hSetBuffering stdin (BlockBuffering (Just bufSize))
    runResourceT $ runConduit $
        sourceHandle stdin .|
        intoCSV csvSettings .|
        csvToJson .|
        sinkHandle stdout
