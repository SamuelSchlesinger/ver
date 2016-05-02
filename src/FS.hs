{-# LANGUAGE DeriveGeneric #-}

module FS (Repo(..), getRepo, findHead, fsProgram) where

import GHC.Generics
import Control.Monad
import System.Directory
import System.FilePath
import Data.ByteString (writeFile)
import Data.Aeson
import Feature

fsProgram = Program "fs" [headCommand] "head"

headCommand = Command "head" headf ""

headf :: [String] -> IO ()
headf _ = do
    wd <- getCurrentDirectory
    h <- findHead wd
    putStrLn h

data Repo = Directory { path :: FilePath, contents :: [Repo] }
          | File { path :: FilePath }
          | DoesntExist
          deriving (Generic, Show)

instance FromJSON Repo
instance ToJSON Repo

findHead :: FilePath -> IO FilePath
findHead current = do 
    abscurrent <- (makeAbsolute current)
    findHead' abscurrent 

cutDir :: String -> String -- takes an absolute file path and cuts a directory off
cutDir = reverse . cutDir' . reverse where
               cutDir' [] = []
               cutDir' ('/':[]) = "/"
               cutDir' (x:xs) | x == '/' = xs
                              | otherwise = cutDir' xs

findHead' :: FilePath -> IO FilePath
findHead' [] = return []
findHead' current = do -- can expect to receive an absolute path
    dumbContents <- getDirectoryContents current
    if ".ver" `elem` dumbContents
        then return current
        else if (current == "/")
                then return "/"
                else do newpath <- (return . cutDir) current
                        findHead' newpath

getRepo :: FilePath -> IO Repo
getRepo path = do
    isDirectory <- doesDirectoryExist path
    if isDirectory
        then do
            dumbContents <- getDirectoryContents path
            contents <- filterM (return . (`notElem` [".", ".."]))
                                dumbContents
            abscontents <- mapM (makeAbsolute . (path </>)) contents
            repos <- mapM getRepo abscontents
            return $ Directory path repos
        else do
            isFile <- doesFileExist path
            if isFile
              then return $ File path
              else return $ DoesntExist                 
