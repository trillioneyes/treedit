{-# LANGUAGE GADTs #-}
module Lib where

data Tree' tag = T tag [Tree' tag]
  deriving Show
data Context' tag = C tag [Tree' tag] [Tree' tag]
  deriving Show

type Tree = Tree' String
type Context = Context' String

newTree :: a -> Tree' a
newTree a = T a []

type Cursor' tag = ([Context' tag], Tree' tag)
type Cursor = Cursor' String

up :: Cursor' a -> Cursor' a
up ([], v) = ([], v)
up (C tag l r:cs, v) = (cs, T tag (reverse l ++ [v] ++ r))

insertUp :: a -> Cursor' a -> Cursor' a
insertUp tag (cs, t) = (cs, T tag [t])

down :: Cursor' a -> Cursor' a
down (cs, T tag (child:children)) = (C tag [] children:cs, child)
down (cs, T tag []) = (cs, newTree tag)

insertDown :: a -> Cursor' a -> Cursor' a
insertDown tag (cs, T tag' children) = (C tag' [] children:cs, newTree tag)

next :: Cursor' a -> Cursor' a
next (C tag l (r:rs):cs, t) = (C tag (t:l) rs:cs, r)
next c = c

insertNext :: a -> Cursor' a -> Cursor' a
insertNext tag (C tag' l r:cs, t) = (C tag' (t:l) r:cs, newTree tag)
insertNext _ ([], t) = ([], t)

previous :: Cursor' a -> Cursor' a
previous (C tag (l:ls) r:cs, t) = (C tag ls (t:r):cs, l)
previous c = c

insertPrevious :: a -> Cursor' a -> Cursor' a
insertPrevious tag (C tag' l r:cs, t) = (C tag' l (t:r):cs, newTree tag)
insertPrevious _ ([], t) = ([], t)

delete :: Cursor' a -> Cursor' a
delete (C tag l (r:rs):cs, _) = (C tag l rs:cs, r)
delete (C tag (l:ls) []:cs, _) = (C tag ls []:cs, l)
delete (C tag [] []:cs, _) = (cs, T tag [])
delete ([], _) = ([], newTree undefined)

rename :: a -> Cursor' a -> Cursor' a
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
  "insert" : name -> insertNext (unwords name)
  "insertPrev" : name -> insertPrevious (unwords name)
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
main = edit ([], newTree "START HERE") >> return ()
