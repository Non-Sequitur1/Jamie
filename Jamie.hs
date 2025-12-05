-- Copyright 2025 (C), The Church of Jamie Willis. 

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (stdout)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Control.Applicative
import VM (runPure)
import Parse
import Grammar

instructions :: Parser [Instruction]
instructions = many instruction
    where
        instruction :: Parser Instruction
        instruction =
            (string "jamie " *> pure IncDataPoint) -- parse increment data point
            <|> (string "scala " *> pure DecDataPoint) -- parse decrement data point
            <|> (string "parser " *> pure IncDataVal) -- parse increment data value
            <|> (string "parsley " *> pure DecDataVal) -- parse decrement data value
            <|> (string "cult of nick wu " *> pure OutDataVal) -- parse output data value
            <|> (string "age is just a number " *> pure AcceptOneByteIn) -- parse accept a byte of input
            <|> (WhileDataValNonZero <$> (string "phd in parsing " *> instructions <* string "sam microscopic ")) -- parse loop while data value isn't zero

main :: IO ()
main = do
  args <- getArgs
  case args of
    [progFile] -> runWith progFile Nothing
    [progFile, inputFile] -> runWith progFile (Just inputFile)
    _ -> die "Usage: Jamie <program.txt> [input.bin]"

runWith :: FilePath -> Maybe FilePath -> IO ()
runWith progFile mInputFile = do
  progSrc <- readFile progFile
  case parse instructions progSrc of
    [] -> die "Parse error: program did not match grammar."
    (prog, leftover):_ -> do
      if not (null leftover)
        then putStrLn $ "Warning: leftover after parse: " ++ show leftover
        else return ()

      inputBytes <- case mInputFile of
        Nothing        -> pure []
        Just inputFile -> BS.unpack <$> BS.readFile inputFile

      let outBytes = runPure prog inputBytes

      -- Write raw bytes to stdout
      BS.hPut stdout (BS.pack outBytes)

bytesToAscii :: [Word8] -> String
bytesToAscii = map (\w -> if w >= 32 && w <= 126 then toEnum (fromIntegral w) else '.')

