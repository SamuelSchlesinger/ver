module Snap (snapCommand) where

import Prelude hiding (writeFile, readFile)
import Feature
import Fact
import FS
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import System.Directory
import System.FilePath
import Data.ByteString.Lazy hiding (putStrLn, map, tail, filter)
import Codec.Archive.Zip
import Data.String.Utils

snapCommand = Command "snap" snapf ""

snapf :: [String] -> IO ()

containsSeq :: Eq a => [a] -> [a] -> Bool

containsSeq [] _ = True
containsSeq _ [] = False
containsSeq s@(x':xs')  (x:xs) | x == x' = startswith xs' xs
                               | otherwise = containsSeq s xs

snapf _ = do dir <- getCurrentDirectory >>= makeAbsolute
             headdir <- findHead dir
             let verdir = headdir ++ "/.ver"
             fact <- getFact (headdir ++ "/.ver")
             case findFact "branch" fact of
                 Nothing -> putStrLn "Current working branch not set"
                 Just branch -> do
                    let branchdir = verdir ++ "/branches/" ++ branch
                    time <- getCurrentTime
                    let timestamp = show time
                    let timestamp' = map (\x -> if x == ' ' then '_' 
                                                            else if x == ':' then '+'
                                                                             else  x) timestamp
                    structure <- readFile (branchdir ++ "/structure.json")
                    let repo = (decode structure) :: Maybe Repo
                    case repo of
                        Nothing -> return ()
                        Just repo' -> do
                            let paths = listPaths repo'
                            let paths' = filter (not . (startswith ".ver"))
                                                (map (makeRelative headdir) paths)
                            mapM_ putStrLn (tail paths')
