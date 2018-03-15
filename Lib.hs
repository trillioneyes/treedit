{-# LANGUAGE DeriveGeneric #-}
module Lib where
import Data.Serialize(Serialize)
import GHC.Generics(Generic)
import Control.Applicative

data Tree tag = T tag [Tree tag]
  deriving (Show, Generic)
instance Serialize a => Serialize (Tree a) where

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
  down (cs, c') <|> Just (cs, c')
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

stitch' :: Context a -> Tree a
stitch' c = T (label c) (map stitch' (children c))

stitch :: Cursor a -> Tree a
stitch cur = maybe (stitch' t) stitch (up cur) where
  t = snd cur

unstitch' :: Tree a -> Context a
unstitch' (T tag cs) = C tag [] (map unstitch' cs)

unstitch :: Tree a -> Cursor a
unstitch t = ([], unstitch' t)
