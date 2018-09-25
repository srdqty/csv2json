module CommandLineOptions
    ( Options (Options)
    , parseCommandLineOptions
    , csvSettings
    , inputHandle
    , outputHandle
    ) where

import Data.CSV.Conduit (CSVSettings, defCSVSettings, csvQuoteChar, csvSep)
import Data.Semigroup ((<>))
import Options.Applicative
    ( Parser
    , (<**>)
    , auto
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , optional
    , progDesc
    , short
    , showDefault
    , value
    )
import System.IO
    ( BufferMode (BlockBuffering)
    , Handle
    , IOMode (ReadMode, WriteMode)
    , hSetBuffering
    , openFile
    , stdin
    , stdout
    )

data Options = Options
    Char
    (Maybe Char)
    Int
    Int
    (Maybe FilePath)
    (Maybe FilePath)
    deriving (Eq, Ord, Show)

csvSettings :: Options -> CSVSettings
csvSettings (Options sep quoteChar _ _ _ _) =
    defCSVSettings { csvSep = sep, csvQuoteChar = quoteChar }

inputHandle :: Options -> IO Handle
inputHandle (Options _ _ bufSize _ Nothing _) = do
    hSetBuffering stdin (BlockBuffering (Just bufSize))
    return stdin
inputHandle (Options _ _ bufSize _ (Just path) _) = do
    handle <- openFile path ReadMode
    hSetBuffering handle (BlockBuffering (Just bufSize))
    return handle

outputHandle :: Options -> IO Handle
outputHandle (Options _ _ _ bufSize _ Nothing) = do
    hSetBuffering stdout (BlockBuffering (Just bufSize))
    return stdout
outputHandle (Options _ _ _ bufSize _ (Just path)) = do
    handle <- openFile path WriteMode
    hSetBuffering handle (BlockBuffering (Just bufSize))
    return handle

parseCommandLineOptions :: IO Options
parseCommandLineOptions = execParser opts
    where
        opts = info (optionsParser <**> helper)
            ( fullDesc
           <> progDesc "Convert CSV to JSONL"
           <> header "csv2json - conver CSV to JSONL"
            )

optionsParser :: Parser Options
optionsParser = Options
    <$> csvSeparatorParser
    <*> csvQuoteCharParser
    <*> inputBufferSizeParser
    <*> outputBufferSizeParser
    <*> inputFilenameParser
    <*> outputFilenameParser

csvSeparatorParser :: Parser Char
csvSeparatorParser = option auto
    ( long "csv-separator"
   <> short 's'
   <> metavar "CSV_SEPARATOR"
   <> showDefault
   <> value ','
   <> help "Character that separates csv columns." )

csvQuoteCharParser :: Parser (Maybe Char)
csvQuoteCharParser = optional $ option auto
    ( long "csv-quote-char"
   <> short 'q'
   <> metavar "CSV_QUOTE_CHAR"
   <> help "Character that quotes csv columns." )

inputBufferSizeParser :: Parser Int
inputBufferSizeParser = option auto
    ( long "input-buffer-size"
   <> metavar "INPUT_BUFFER_SIZE"
   <> showDefault
   <> value (2*1024*1024)
   <> help "Size of the input handle buffer")

outputBufferSizeParser :: Parser Int
outputBufferSizeParser = option auto
    ( long "output-buffer-size"
   <> metavar "OUTPUT_BUFFER_SIZE"
   <> showDefault
   <> value (2*1024*1024)
   <> help "Size of the output handle buffer")

inputFilenameParser :: Parser (Maybe FilePath)
inputFilenameParser = optional $ option auto
    ( long "input-filename"
   <> short 'i'
   <> metavar "INPUT_FILENAME"
   <> help "Input filename." )

outputFilenameParser :: Parser (Maybe FilePath)
outputFilenameParser = optional $ option auto
    ( long "output-filename"
   <> short 'o'
   <> metavar "OUTPUT_FILENAME"
   <> help "Output filename." )
