import System.Environment
import System.Directory
import System.Posix.Files
import qualified Data.ByteString.Lazy as L

-- open the file
-- extract the relationships from the file
-- create the files
main :: IO ()
main = do
    files <- getArgs
    mapM_ handleFile files

handleFile :: String -> IO ()
handleFile file = do
    currentDirectory <- getCurrentDirectory
    homeDirectory <- getHomeDirectory
    contents <- L.readFile file
    return ()
