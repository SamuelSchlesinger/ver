module Snap (snapCommand) where

import Prelude hiding (writeFile)
import Feature
import Fact
import FS
import Control.Monad
import Data.Aeson
import System.Directory
import Data.ByteString.Lazy

snapCommand = Command "snap" snapf ""

snapf :: [String] -> IO ()

snapf _ = do dir <- getCurrentDirectory
             let verdir = dir ++ "/.ver"
             fact <- getFact verdir

initdir dirname= do createDirectoryIfMissing True dirname
                     let verdir = dirname ++ "/.ver"
                     createDirectoryIfMissing True verdir
                     createDirectoryIfMissing True (verdir ++ "/branches")
                     createDirectoryIfMissing True (verdir ++ "/branches/master")
                     factEncoding <- (return $ UniFact "branch" "master") 
                                 >>= (return . toJSON) 
                                 >>= (return . encode)
                     writeFile (verdir ++ "/facts.json")
                               factEncoding
                     structureEncoding <- getRepo dirname 
                                     >>= (return . toJSON) 
                                     >>= (return . encode)
                     writeFile (verdir ++ "/branches/master/structure.json")
                               structureEncoding
