{-# LANGUAGE GADTs #-}
module Lib where

data Tree = T String [Tree]
  deriving Show
data Context = C String [Tree] [Tree]
  deriving Show

newTree :: Tree
newTree = T "" []

emptyContext :: Context
emptyContext = C "" [] []

type Cursor = ([Context], Tree)

up :: Cursor -> Cursor
up ([], v) = ([], T "" [v])
up ((C tag l r:cs), v) = (cs, T tag (reverse l ++ [v] ++ r))

down :: Cursor -> Cursor
down (cs, T tag (child:children)) = (C tag [] children:cs, child)
down (cs, T tag []) = (C tag [] []:cs, newTree)

next :: Cursor -> Cursor
next (C tag l (r:rs):cs, t) = (C tag (t:l) rs:cs, r)
next (C tag l []:cs, t) = (C tag (t:l) []:cs, newTree)
next c = insert c

previous :: Cursor -> Cursor
previous (C tag (l:ls) r:cs, t) = (C tag ls (t:r):cs, l)
previous (C tag [] r:cs, t) = (C tag [] (t:r):cs, newTree)
previous c = insert c

insert :: Cursor -> Cursor
insert (C tag l r:cs, t) = (C tag l (t:r):cs, newTree)
insert ([], T tag vals) = ([C tag [] vals], newTree)

delete :: Cursor -> Cursor
delete (C tag l (r:rs):cs, _) = (C tag l rs:cs, r)
delete (C tag (l:ls) []:cs, _) = (C tag ls []:cs, l)
delete (C tag [] []:cs, _) = (cs, T tag [])

rename :: String -> Cursor -> Cursor
rename tag (cs, T _ vals) = (cs, T tag vals)

(!>>) = flip ($)

dataCursor :: Cursor
dataCursor = ([], newTree) !>> rename "TOPLEVEL" !>> next !>> rename "leaf" !>>
  next !>> rename "branch" !>> next !>> rename "another leaf" !>> previous !>>
  down !>> rename "subtree" !>> down !>> rename "deep leaf" !>> up !>> insert
