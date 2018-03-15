module Treedit.IO
    (
      readCursor,
      writeCursor
    ) where
import Prelude hiding (readFile, writeFile)
import Data.Serialize(encode, decode, Serialize)
import Data.ByteString
import Lib

readCursor :: Serialize a => FilePath -> IO (Maybe (Cursor a))
readCursor path = do
  s <- readFile path
  case decode s of
    Left err -> Prelude.putStrLn err >> return Nothing
    Right tree -> return . Just $ unstitch tree

writeCursor :: Serialize a => FilePath -> Cursor a -> IO ()
writeCursor path cur = writeFile path $ encode (stitch cur)
