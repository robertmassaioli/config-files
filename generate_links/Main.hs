import System.Environment
import System.Directory
import System.Posix.Files
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Text.Delimited
import System.FilePath

-- open the file
-- extract the relationships from the file
-- create the files
main :: IO ()
main = do
    info <- getInfo
    files <- getArgs
    mappings <- mapM (handleFile info) $ map (makeRelative $ curDir info) files
    generateLinks (concat mappings) info

data Mapping = Mapping {
                mappingTo :: String
                , mappingFrom :: String
                }
               deriving (Show, Eq)

data SysInfo = Info {
                homeDir :: FilePath
                , curDir :: FilePath
               }
               deriving (Show)

getInfo :: IO (SysInfo)
getInfo = do
    home <- getHomeDirectory
    cur <- getCurrentDirectory
    return $ Info { homeDir = home, curDir = cur }

handleFile :: SysInfo -> String -> IO [Mapping]
handleFile info file = do
    contents <- L.readFile file
    case decode dels contents of
        Left _ -> return []
        Right a -> mapM toMapping a
    where
        toMapping :: Record -> IO Mapping
        toMapping (from:to:_) = do
            fromFull <- canonicalizePath . checkHome . C.unpack $ from
            toFull <- canonicalizePath . checkHome . C.unpack $ to
            return $ Mapping { mappingTo = toFull, mappingFrom = fromFull }
            
        checkHome :: String -> String
        checkHome []        = []
        checkHome ('~':xs)  = homeDir info ++ xs
        checkHome xs        = xs

        dels :: String
        dels = "|"

generateLinks :: [Mapping] -> SysInfo -> IO ()
generateLinks mapping info = mapM_ genMap mapping
    where
        genMap :: Mapping -> IO ()
        genMap mapping = tryCreateSymbolicLink (mappingFrom mapping) (mappingTo mapping)

tryCreateSymbolicLink :: FilePath -> FilePath -> IO ()
tryCreateSymbolicLink from to = do
    toExists <- fileExist to
    fromExists <- fileExist from
    if (not toExists) && fromExists 
        then
            createSymbolicLink from to
         else
            return ()
