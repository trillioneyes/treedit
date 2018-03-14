module Main (main) where
import Lib
import Control.Monad(void)
import Data.Maybe(fromMaybe)

main :: IO ()
main = void $ edit ([], context "START HERE")

command :: String -> Cursor String -> Maybe (Cursor String)
command line = case words line of
  ["up"] -> up
  ["down"] -> down
  ["next"] -> next
  ["previous"] -> previous
  "rename" : name -> Just . rename (unwords name)
  "insert" : name -> insertNext (unwords name)
  "insertPrev" : name -> insertPrevious (unwords name)
  "insertUp" : name -> Just . insertUp (unwords name)
  "insertDown" : name -> Just . insertDown (unwords name)
  ["delete"] -> delete
  _ -> Just

edit :: Cursor String -> IO (Cursor String)
edit c = do
  putStr (printCursor c)
  line <- getLine
  case line of
    "" -> return c
    _ -> edit (fromMaybe c (command line c))

treeLines :: Tree (Style, String) -> [String]
treeLines (T tag' cs) = render tag' : (cs >>= childLines) where
    render (Selected, tag) = "'" ++ tag ++ "'**:"
    render (Tag, tag) = "'" ++ tag ++ "':"
    render (Text, tag) = "'" ++ tag ++ "'"
    childLines = map ("  "++) . treeLines

printCursor :: Cursor String -> String
printCursor = unlines . treeLines . stitch . styleCursor

data Style = Text | Selected | Tag

stylize :: [a] -> Style
stylize [] = Text
stylize _ = Tag

styleContext :: Context String -> Context (Style, String)
styleContext cur@(C tag ls rs) =
  C (stylize (children cur), tag) (map styleContext ls) (map styleContext rs)

styleCursor :: Cursor String -> Cursor (Style, String)
styleCursor (cs, C tag ls rs) =
   (map styleContext cs,
    C (Selected, tag) (map styleContext ls) (map styleContext rs))
