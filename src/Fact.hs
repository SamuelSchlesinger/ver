{-# LANGUAGE DeriveGeneric #-}

module Fact (Fact(UniFact, EmptyFact), getFact, findFact, replaceFact, factProgram) where

import Feature
import FS
import GHC.Generics
import Control.Monad
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Aeson

factProgram = Program "fact" [setFactCommand, getFactCommand, listFactCommand] "set | get"

setFactCommand = Command "set" setFactf "<fact-name> <fact-value>"
getFactCommand = Command "get" getFactf "{<fact-name>}"
listFactCommand = Command "list" listFactf ""

setFactf :: [String] -> IO ()
setFactf (name:value:_) = do
    wd <- getCurrentDirectory
    headdir <- findHead wd
    fact <- getFact headdir
    if hasFact name fact 
        then let fact' = replaceFact name value fact in
             writeFact (headdir ++ "/.ver") fact'
        else let fact' = MultiFact [fact, (UniFact name value)] in
             writeFact (headdir ++ "/.ver") fact'
setFactf _ = putStrLn "Missing arguments to fact set"

listFactf _ = putStrLn "Unimplemented"

getFactf :: [String] -> IO ()
getFactf [] = return ()
getFactf (name:xs) = do
    wd <- getCurrentDirectory
    headdir <- findHead wd
    fact <- getFact (headdir ++ "/.ver")
    let toPrint = case findFact name fact of
                      Nothing -> ""
                      Just value' -> name ++ ": " ++ value' ++ "\n"
    putStr toPrint
    getFactf xs

{-
listFactf :: [String] -> IO ()
listFactf _ = do
    wd <- getCurrentDirectory
    headdir <- findHead wd
    fact <-  getFact headdir
    forM_ (\(x, y) -> putStrLn (x ++ ": " ++ y)) (return (flatten fact))  
-}

data Fact = UniFact { name :: String, value :: String }
          | MultiFact { facts :: [Fact] }
          | EmptyFact
          deriving (Generic, Show)

flatten :: Fact -> [(String, String)]
flatten (UniFact name value) = [(name, value)]
flatten (MultiFact facts) = case facts of
                                [] -> []
                                (x:xs) -> (flatten x) ++ (flatten' xs) where
                                    flatten' [] = []
                                    flatten' (x:xs) = (flatten x) ++ (flatten' xs)
flatten EmptyFact = []

instance FromJSON Fact
instance ToJSON Fact

getFact :: FilePath -> IO Fact
getFact path = do
    isDirectory <- doesDirectoryExist path
    if isDirectory then do
                       let factpath = path ++ "/facts.json"
                       hasFacts <- doesFileExist factpath
                       if hasFacts then do
                           contents <- readFile factpath
                           return $ case decode contents of
                                        Nothing -> EmptyFact
                                        Just a -> a
                                   else return EmptyFact
                   else return EmptyFact

writeFact path fact = do
    isDirectory <- doesDirectoryExist path
    if isDirectory then do
                       let factpath = path ++ "/facts.json"
                       factEncoding <- (return fact)
                                   >>= (return . toJSON)
                                   >>= (return . encode)
                       writeFile (path ++ "/facts.json")
                                 factEncoding
                   else return ()
notNothing :: Maybe a -> Bool
notNothing (Just _) = True
notNothing Nothing = False

findFact :: String -> Fact -> Maybe String
findFact name (UniFact name' value) = if name == name' then Just value else Nothing
findFact name mf@(MultiFact facts) = let flatfacts = flatten mf in
                                  lookup name flatfacts
findFact _ EmptyFact = Nothing

hasFact :: String -> Fact -> Bool
hasFact name (UniFact name' value') = name == name'
hasFact name EmptyFact = False
hasFact name (MultiFact facts) = foldr (||) False (map (hasFact name) facts)

replaceFact :: String -> String -> Fact -> Fact
replaceFact name value u@(UniFact name' value') | name == name' = UniFact name value
                                                | otherwise = u
replaceFact name value (MultiFact facts) = MultiFact (map (replaceFact name value) facts)

replaceFact _ _ EmptyFact = EmptyFact
