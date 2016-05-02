module Feature where

import Data.List
import System.IO


-- | A feature which is implemented within a program.
-------------------------------------------------------------------------
--   A feature can be a Command, executing an IO action given arguments
-------------------------------------------------------------------------
--   A feature can also be a Program, extracting the arguments and
--      beginning the process
-------------------------------------------------------------------------  

data Feature = Command String ([String] -> IO ()) String
             | Program String [Feature] String

mkCommand :: String -> ([String] -> IO ()) -> String -> Feature
mkCommand = Command

mkProgram :: String -> [Feature] -> String -> Feature
mkProgram = Program

getFeature :: String -> [Feature] -> Maybe Feature
getFeature x = find (\f-> case f of
                              (Command name cmd info) -> name == x
                              (Program name features info) -> name == x)

copyN :: Int -> String -> String
copyN n s = copyN' n s "" where
                copyN' 0 s k = k
                copyN' n s k | n > 0 = copyN' (n-1) s (s ++ k)
                             | n < 0 = ""

usage :: [Feature] -> String
usage features = "Usage: \n" ++ usage' 1 features where
                    usage' _ [] = []
                    usage' d ((Command name _ info):xs) = (copyN d "   ") ++ name 
                                                     ++ " " ++ info 
                                                     ++ "\n" ++ (usage' d xs)
                    usage' d ((Program name features' info):xs) = (copyN d "   ") ++ name
                                                     ++ " " ++ info
                                                     ++ "\n" ++ (usage' (d + 1) features')
                                                     ++ (usage' d xs)

run :: String -> [Feature] -> [String] -> IO ()
run name features args = case getFeature name features of
                           Just (Command _ cmd _) -> cmd args
                           Just (Program _ features' _) -> case args of
                                                               [] -> putStr $ usage features
                                                               (x:xs) -> run x features' xs
                           Nothing -> putStr $ usage features
