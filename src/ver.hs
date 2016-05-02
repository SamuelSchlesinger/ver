import System.Environment
import System.IO
import Feature
import Init
import Fact
import FS
import Snap

features :: [Feature]
features = [initCommand, factProgram, fsProgram, snapCommand]

main = do args <- getArgs
          case args of 
              []     -> run "" features []
              (x:xs) -> run x features xs
