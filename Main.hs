module Main (main) where
import Lib
import Data.Maybe(fromMaybe)
import System.Environment
import Treedit.IO
import qualified Treedit.Style as Style

main :: IO ()
main = do
  args <- getArgs
  cur <- edit Style.pseudoPy =<< loadCursor args
  saveCursor args cur
loadCursor :: [String] -> IO (Cursor String)
loadCursor (path:_) = fromMaybe (cursor "") `fmap` readCursor path
loadCursor [] = return $ cursor ""
saveCursor :: [String] -> Cursor String -> IO ()
saveCursor (path:_) = writeCursor path
saveCursor [] = const (return ())


command :: String -> Cursor String -> Maybe (Cursor String)
command line = case words line of
  ["h"] -> previous
  "H" : name -> insertPrevious (unwords name)
  ["sh"] -> swapPrevious
  ["mh"] -> movePrevious
  ["j"] -> down
  ["sj"] -> swapDown
  ["mj"] -> moveDown
  "J" : name -> Just . insertDown (unwords name)
  ["k"] -> up
  ["sk"] -> swapUp
  ["mk"] -> moveUp
  "K" : name -> Just . insertUp (unwords name)
  ["l"] -> next
  ["sl"] -> swapNext
  ["ml"] -> moveNext
  "L" : name -> insertNext (unwords name)
  "r" : name -> Just . rename (unwords name)
  ["x"] -> delete
  _ -> Just

edit :: Style.Rules -> Cursor String -> IO (Cursor String)
edit layOut c = do
  putStr (printCursor layOut c)
  line <- getLine
  case line of
    "exit" -> return c
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
