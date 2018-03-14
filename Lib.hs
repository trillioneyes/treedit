{-# LANGUAGE GADTs #-}
module Lib where
import Control.Monad
import Data.Maybe

data Tree tag = T tag [Tree tag]
  deriving Show
data Context tag = C tag [Context tag] [Context tag]
  deriving Show

type Cursor tag = ([Context tag], Context tag)

children :: Context a -> [Context a]
children (C _ ls rs) = reverse ls ++ rs
label :: Context a -> a
label (C tag _ _) = tag
addChild :: Context a -> Context a -> Context a
addChild (C tag ls rs) new = C tag ls (new:rs)
removeChild :: Context a -> Maybe (Context a)
removeChild (C tag ls (_:rs)) = Just $ C tag ls rs
removeChild _ = Nothing
context :: a -> Context a
context a = C a [] []
seekLeft :: Context a -> Maybe (Context a)
seekLeft (C tag (l:ls) rs) = Just $ C tag ls (l:rs)
seekLeft _ = Nothing
seekRight :: Context a -> Maybe (Context a)
seekRight (C tag ls (r:rs)) = Just $ C tag (r:ls) rs
seekRight _ = Nothing
descend :: Context a -> Maybe (Context a)
descend (C _ _ (r:_)) = Just r
descend (C _ _ []) = Nothing

up :: Cursor a -> Maybe (Cursor a)
up ([], _) = Nothing
up (c:cs, v) = Just (cs, addChild c v)
insertUp :: a -> Cursor a -> Cursor a
insertUp tag (cs, t) = (cs, addChild (context tag) t)

down :: Cursor a -> Maybe (Cursor a)
down (cs, t) = do
  t' <- descend t
  c <- removeChild t
  return (c:cs, t')
insertDown :: a -> Cursor a -> Cursor a
insertDown tag (cs, c) = (c:cs, context tag)

modifyUp :: Cursor a -> (Context a -> Maybe (Context a)) -> Maybe (Cursor a)
modifyUp ([], _) _ = Nothing
modifyUp (c:cs, t) f = do
  c' <- f (addChild c t)
  down (cs, c')
modifyUp' :: Cursor a -> (Context a -> Context a) -> Maybe (Cursor a)
modifyUp' c f = modifyUp c (return . f)

next :: Cursor a -> Maybe (Cursor a)
next cur = modifyUp cur seekRight
insertNext :: a -> Cursor a -> Maybe (Cursor a)
insertNext tag cur = modifyUp' cur $ \con -> case seekRight con of
  Nothing -> addChild con (context tag)
  Just con' -> addChild con' (context tag)

previous :: Cursor a -> Maybe (Cursor a)
previous cur = modifyUp cur seekLeft
insertPrevious :: a -> Cursor a -> Maybe (Cursor a)
insertPrevious tag cur = modifyUp' cur (`addChild` context tag)

delete :: Cursor a -> Maybe (Cursor a)
delete cur = modifyUp cur removeChild

rename :: a -> Cursor a -> Cursor a
rename tag (cs, C _ ls rs) = (cs, C tag ls rs)

(!>>) :: a -> (a -> b) -> b
(!>>) = flip ($)

data Style = Text | Selected | Tag

stylize :: [a] -> Style
stylize [] = Text
stylize _ = Tag

styleTree :: Tree String -> Tree (Style, String)
styleTree (T tag cs) = T (stylize cs, tag) (map styleTree cs)
styleContext :: Context String -> Context (Style, String)
styleContext cur@(C tag ls rs) =
  C (stylize (children cur), tag) (map styleContext ls) (map styleContext rs)

styleCursor :: Cursor String -> Cursor (Style, String)
styleCursor (cs, C tag ls rs) =
   (map styleContext cs,
    C (Selected, tag) (map styleContext ls) (map styleContext rs))

stitch' :: Context a -> Tree a
stitch' c = T (label c) (map stitch' (children c))

stitch :: Cursor a -> Tree a
stitch cur = maybe (stitch' t) stitch (up cur) where
  t = snd cur

treeLines :: Tree (Style, String) -> [String]
treeLines (T tag' cs) = render tag' : (cs >>= childLines)
  where
    render (Selected, tag) = "'" ++ tag ++ "'**:"
    render (Tag, tag) = "'" ++ tag ++ "':"
    render (Text, tag) = "'" ++ tag ++ "'"
    childLines = map ("  "++) . treeLines

printCursor :: Cursor String -> String
printCursor = unlines . treeLines . stitch . styleCursor

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

main :: IO ()
main = void $ edit ([], context "START HERE")
