module Treedit.IO
    (
      readTree,
      readCursor,
      writeCursor
    ) where
import Prelude hiding (readFile, writeFile)
import Data.Serialize(encode, decode, Serialize)
import Data.ByteString
import System.Directory
import Lib

readTree :: Serialize a => FilePath -> IO (Maybe (Tree a))
readTree path = do
  exists <- doesFileExist path
  if exists
    then do
      s <- readFile path
      case decode s of
        Left err -> Prelude.putStrLn err >> return Nothing
        Right tree -> return . Just $ tree
    else return Nothing

readCursor :: Serialize a => FilePath -> IO (Maybe (Cursor a))
readCursor path = fmap unstitch <$> readTree path

writeCursor :: Serialize a => FilePath -> Cursor a -> IO ()
writeCursor path cur = writeFile path $ encode (stitch cur)
