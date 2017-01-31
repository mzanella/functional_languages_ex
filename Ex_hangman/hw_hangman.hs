import System.IO

getCh::IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                  do putChar x
                     return []
              else
                  do putChar '-'
                     xs <- sgetLine
                     return (x:xs)

hangman :: IO()
hangman = do putStr "Think a word: "
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

play :: String -> IO()
play word = do putStr "? "
               attempt <- getLine
               if attempt == word then
                   putStrLn "You got it!"
               else
                   do putStrLn (suggestion attempt word)
                      play word

suggestion :: String -> String -> String
suggestion attempt [] = []
suggestion attempt (x:xs) = if x `elem` attempt then 
                                x:(suggestion attempt xs)
                            else 
                                '-':(suggestion attempt xs)