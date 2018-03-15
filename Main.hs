module Main (main) where
import Lib
import Data.Maybe(fromMaybe)
import System.Environment
import Treedit.IO

main :: IO ()
main = do
  path:_ <- getArgs
  maybeC <- readCursor path
  case maybeC of
    Nothing -> return ()
    Just cur -> do
      cur' <- edit cur
      writeCursor path cur'

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
