module Main (main) where
import Lib
import Data.Maybe(fromMaybe)
import Data.Char
import System.Environment
import Treedit.IO
import Treedit.Style

main :: IO ()
main = do
  path:_ <- getArgs
  cur <- edit =<< getCursor path
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

edit :: Cursor String -> IO (Cursor String)
edit c = do
  putStr (printCursor c)
  line <- getLine
  case line of
    "save" -> return c
    _ -> edit (fromMaybe c (command line c))

treeLayout :: Tree (Style, String) -> Layout
treeLayout (T (sty, tag) cs) =
  sty tag (map treeLayout cs)

printCursor :: Cursor String -> String
printCursor = render . treeLayout . stitch . styleCursor

stylize :: String -> Style
stylize tag | all isAlphaNum tag = functionCall
            | otherwise = binOp

styleContext :: Context String -> Context (Style, String)
styleContext (C tag ls rs) =
  C styleTag (map styleContext ls) (map styleContext rs) where
    styleTag = (stylize tag, tag)

styleCursor :: Cursor String -> Cursor (Style, String)
styleCursor (cs, C tag ls rs) =
   (map styleContext cs,
    C (selected (stylize tag), tag) (map styleContext ls) (map styleContext rs))
