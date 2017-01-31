{-# OPTIONS_GHC -Wall #-}
module hw2 where
import Log

data MessageType = Info
                  | Warning
                  | Error Int
                  deriving (Show, Eq)

type Timestamp = Int

data LogMessage = LogMessage MessageType Timestamp String
    | Unknown String
    deriving (Show, Eq)

concatStringWithSpaces :: [String] -> String
concatStringWithSpaces strings
                       | length strings == 0 = ""
                       | length strings == 1 = head strings
                       | otherwise = strings!!0 ++ " " ++ (concatStringWithSpaces (tail strings))

parseMessage :: String -> LogMessage
parseMessage message = if firstWord == "I" then buildInfoFromWords wordsOfTheMessage
                       else if firstWord == "W" then buildWarningFromWords wordsOfTheMessage
                       else if firstWord == "E" then buildErrorFromWords wordsOfTheMessage
                       else Unknown (concatStringWithSpaces wordsOfTheMessage)
                       where wordsOfTheMessage = words message
                             firstWord = head wordsOfTheMessage

buildErrorFromWords :: [String] -> LogMessage
buildErrorFromWords wordsOfTheMessage = LogMessage (Error (read lvl :: Int)) (read tmstmp :: Int) description
                                        where lvl = wordsOfTheMessage!!1
                                              tmstmp = wordsOfTheMessage!!2
                                              description = concatStringWithSpaces (drop 3 wordsOfTheMessage)

buildWarningFromWords :: [String] -> LogMessage
buildWarningFromWords wordsOfTheMessage = LogMessage Warning (read tmstmp :: Int) description
                                          where tmstmp = wordsOfTheMessage!!1
                                                description = concatStringWithSpaces (drop 2 wordsOfTheMessage)

buildInfoFromWords :: [String] -> LogMessage
buildInfoFromWords wordsOfTheMessage = LogMessage Info (read tmstmp :: Int) description
                                       where tmstmp = wordsOfTheMessage!!1
                                             description = concatStringWithSpaces (drop 2 wordsOfTheMessage)

parse :: String -> [LogMessage]
