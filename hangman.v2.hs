-- ___
-- |  |
-- |  o
-- | /|\
-- | / \
-- |

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
             play word [] 15

play :: String -> String -> Int -> IO()
play word previous attemptsRemained = do putStr "? "
                                         attempt <- getLine
                                         if (attempt == word) then
                                             putStrLn "You got it!"
                                         else if (attemptsRemained == 0) then
                                         	 putStrLn "You loose!"
                                         else
                                             do putStrLn (suggestion (attempt++previous) word)
                                                printHangman (15 - (attemptsRemained - 1))
                                                play word (attempt++previous) (attemptsRemained - 1)

suggestion :: String -> String -> String
suggestion attempt [] = []
suggestion attempt (x:xs) = if x `elem` attempt then 
                                x:(suggestion attempt xs)
                            else 
                                '-':(suggestion attempt xs)

printHangman :: Int -> IO()
printHangman attempts = do if attempts == 0 then
								do putStr ""
						   else if 0 < attempts && attempts <=5 then
						   	    do putStrLn "|"
						   	       printHangman (attempts - 1)
				   	       else if (attempts == 6) then
				   	       	    do putStrLn "_"
				   	       	       printHangman (attempts - 1)
				   	       else if ((attempts == 7) || (attempts == 8)) then
				   	       	    do putStr "_"
				   	       	       printHangman (attempts - 1)
		   	       	       else if (attempts == 9) then
				   	       	    do putStrLn "___\n|  |"
				   	       	       printHangman (4)
		   	       	       else if (attempts == 10) then
				   	       	    do putStrLn "___\n|  |\n|  o"
				   	       	       printHangman (3)
		   	       	       else if (attempts == 11) then
				   	       	    do putStrLn "___\n|  |\n|  o\n| /"
				   	       	       printHangman (2)
		   	       	       else if (attempts == 12) then
				   	       	    do putStrLn "___\n|  |\n|  o\n| /|"
				   	       	       printHangman (2)
		   	       	       else if (attempts == 13) then
				   	       	    do putStrLn "___\n|  |\n|  o\n| /|\\"
				   	       	       printHangman (2)
		   	       	       else if (attempts == 14) then
				   	       	    do putStrLn "___\n|  |\n|  o\n| /|\\\n| /"
				   	       	       printHangman (1)
				   	       else if (attempts == 15) then
				   	       	    do putStrLn "___\n|  |\n|  o\n| /|\\\n| / \\"
				   	       	       printHangman (1)
		   	       	       else 
		   	       	       	    do putStr ""
