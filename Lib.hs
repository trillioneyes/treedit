{-# LANGUAGE GADTs #-}
module Lib where

data Tree' tag = T tag [Tree' tag]
  deriving Show
data Context' tag = C tag [Tree' tag] [Tree' tag]
  deriving Show

type Tree = Tree' String
type Context = Context' String

newTree :: Tree
newTree = T "" []

type Cursor' tag = ([Context' tag], Tree' tag)
type Cursor = Cursor' String

up :: Cursor -> Cursor
up ([], v) = ([], T "" [v])
up (C tag l r:cs, v) = (cs, T tag (reverse l ++ [v] ++ r))

down :: Cursor -> Cursor
down (cs, T tag (child:children)) = (C tag [] children:cs, child)
down (cs, T tag []) = (C tag [] []:cs, newTree)

next :: Cursor -> Cursor
next (C tag l (r:rs):cs, t) = (C tag (t:l) rs:cs, r)
next (C tag l []:cs, t) = (C tag (t:l) []:cs, newTree)
next c = c

previous :: Cursor -> Cursor
previous (C tag (l:ls) r:cs, t) = (C tag ls (t:r):cs, l)
previous (C tag [] r:cs, t) = (C tag [] (t:r):cs, newTree)
previous c = c

insert :: String -> Cursor -> Cursor
insert newTag (C tag l r:cs, t) = (C tag l (t:r):cs, T newTag [])
insert newTag ([], T tag vals) = ([C tag [] vals], T newTag [])

insertPrev :: String -> Cursor -> Cursor
insertPrev tag = insert tag . previous

delete :: Cursor -> Cursor
delete (C tag l (r:rs):cs, _) = (C tag l rs:cs, r)
delete (C tag (l:ls) []:cs, _) = (C tag ls []:cs, l)
delete (C tag [] []:cs, _) = (cs, T tag [])
delete ([], _) = ([], newTree)

rename :: String -> Cursor -> Cursor
rename tag (cs, T _ vals) = (cs, T tag vals)

(!>>) :: a -> (a -> b) -> b
(!>>) = flip ($)

data Style = Text | Selected | Tag

stylize :: [a] -> Style
stylize [] = Text
stylize _ = Tag

styleTree :: Tree -> Tree' (Style, String)
styleTree (T tag children) = T (stylize children, tag) (map styleTree children)

styleCursor :: Cursor -> Cursor' (Style, String)
styleCursor (cs, T tag children) =
   (map boringStyle cs, T (Selected, tag) (map styleTree children))
  where
    boringStyle :: Context -> Context' (Style, String)
    boringStyle (C t l r) = C (Tag, t) (map styleTree l) (map styleTree r)

stitch :: Cursor' a -> Tree' a
stitch ([], t) = t
stitch (C tag l r:cs, t) = stitch (cs, T tag (reverse l ++ [t] ++ r))

treeLines :: Tree' (Style, String) -> [String]
treeLines (T tag' children) = render tag' : (children >>= childLines)
  where
    render (Selected, tag) = "**'" ++ tag ++ "':"
    render (Tag, tag) = "'" ++ tag ++ "':"
    render (Text, tag) = "'" ++ tag ++ "'"
    childLines = map ("  "++) . treeLines

printCursor :: Cursor -> String
printCursor = unlines . treeLines . stitch . styleCursor

command :: String -> Cursor -> Cursor
command line = case words line of
  ["up"] -> up
  ["down"] -> down
  ["next"] -> next
  ["previous"] -> previous
  "rename" : name -> rename (unwords name)
  "releaf" : name -> up . rename (unwords name) . down
  "insert" : name -> insert (unwords name)
  "insertPrev" : name -> insertPrev (unwords name)
  ["delete"] -> delete
  _ -> id

edit :: Cursor -> IO Cursor
edit c = do
  putStr (printCursor c)
  line <- getLine
  case line of
    "" -> return c
    _ -> edit (command line c)

main :: IO ()
main = edit ([], newTree) >> return ()
