module Main (main) where
import Lib
import Data.Maybe(fromMaybe)
import System.Environment
import Treedit.IO
import qualified Treedit.Style as Style

main :: IO ()
main = do
  path:_ <- getArgs
  cur <- edit Style.pseudoPy =<< getCursor path
  writeCursor path cur
getCursor :: String -> IO (Cursor String)
getCursor path = fromMaybe (cursor "") `fmap` readCursor path

command :: String -> Cursor String -> Maybe (Cursor String)
command line = case words line of
  ["k"] -> up
  ["j"] -> down
  ["l"] -> next
  ["h"] -> previous
  "r" : name -> Just . rename (unwords name)
  "L" : name -> insertNext (unwords name)
  "H" : name -> insertPrevious (unwords name)
  "K" : name -> Just . insertUp (unwords name)
  "J" : name -> Just . insertDown (unwords name)
  ["x"] -> delete
  _ -> Just

edit :: Style.Rules -> Cursor String -> IO (Cursor String)
edit layOut c = do
  putStr (printCursor layOut c)
  line <- getLine
  case line of
    "save" -> return c
    "view simple" -> edit Style.simple c
    "view py" -> edit Style.pseudoPy c
    _ -> edit layOut (fromMaybe c (command line c))

printCursor :: Style.Rules -> Cursor String -> String
printCursor layOut = Style.render layOut . stitch . styleCursor

styleContext :: Context String -> Context (Style.Selectable String)
styleContext (C tag ls rs) =
  C styleTag (map styleContext ls) (map styleContext rs) where
    styleTag = Style.NoSelect tag

styleCursor :: Cursor String -> Cursor (Style.Selectable String)
styleCursor (cs, C tag ls rs) =
   (map styleContext cs,
    C (Style.Select tag) (map styleContext ls) (map styleContext rs))
