import Data.Bits
import Data.Char
import Data.List
import System.IO
import System.Random

board :: [Int] -> [String]
board [] = []
board(x:xs) = take x (repeat '*') : board xs

display :: [String] -> String
display x = unlines(zipWith (++) ["0:","1:","2:","3:","4:"]x)

makeMove :: Int -> Int -> [Int] -> [Int] 
makeMove r s = (\(a,b:c) -> a ++ b - s:c) . splitAt r

valid :: Int -> Int -> [Int] -> Bool
valid r s newLs = and [r >= 0,  s >= 1, r < length newLs, s <= newLs!!r]
   
getMove :: [Int] -> IO()
getMove newLs = do
                putStr"\nEnter row which you want to remove stars from: "
                row <- getChar
                if isDigit row
                   then putStr""
                   else getMove newLs
                putStr"\nEnter amount of stars you want to remove: "
                stars <- getChar
                if isDigit stars
                   then putStr""
                   else getMove newLs
                putStr"\n"
                if valid(digitToInt row)(digitToInt stars)newLs
                then getMoveV row stars newLs
                else getMove newLs

getMoveV :: Char -> Char -> [Int] -> IO ()
getMoveV row stars newLs = do let newerlist = makeMove(digitToInt row)(digitToInt stars)newLs
                              print newerlist
                              putStr"\n"
                              let string=display (board(newerlist))
                              putStrLn string
                              if sum(newLs) == 1
                                 then putStr"You win!\n"
                                 else getComp newerlist


getComp :: [Int] -> IO ()
getComp newLs = do 
                r <- getStdRandom (randomR (0,4)) 
                s <- getStdRandom (randomR (1,5))
                if (valid r s newLs)
                   then do let newerLs = makeMove r s newLs
                           if xorAll newerLs == 0 
                              then compMove r s newerLs
                              else getComp newLs
                else getComp newLs
     
compMove :: (Show a1, Ord a, Num a) => a1 -> a -> [Int] -> IO ()
compMove r s newerLs = do
                       putStr"\nComp move\n"
                       print newerLs
                       if s >1
                          then putStrLn.concat$["I removed ", show (s),
                                       " stars from row ", show (r)]
                          else putStrLn.concat$["I removed ", show (s),
                                       " star from row ", show (r)]
                       let string = display (board(newerLs))
                       putStrLn string
                       if sum(newerLs) ==0 
                          then putStr"I win!\n"
                          else getMove newerLs

xorAll :: Bits b => [b] -> b
xorAll ls = foldl xor 0 ls

getPlayer :: IO ()
getPlayer = do
            putStr"Would you like to go first?(y/n): "
            choice <- getChar
            if (choice == 'y')
            then getMove [5,4,3,2,1]
            else getComp [5,4,3,2,1]

main = do 
       putStrLn("\nWelcome to nim game!\n")
       let string=display (board([5,4..1]))
       putStrLn string
       getPlayer
