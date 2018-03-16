{-# LANGUAGE DeriveGeneric, TupleSections #-}
module Lib where
import Data.Serialize(Serialize)
import GHC.Generics(Generic)
import Control.Applicative
import Control.Monad

try :: Alternative f => (a -> f a) -> a -> f a
try f a = f a <|> pure a

data Tree tag = T tag [Tree tag]
  deriving (Show, Generic)
instance Serialize a => Serialize (Tree a) where

instance Functor Tree where
  fmap f (T tag ts) = T (f tag) (map (fmap f) ts)

data Context tag = C tag [Context tag] [Context tag]
  deriving Show

type Cursor tag = ([Context tag], Context tag)
cursor :: tag -> Cursor tag
cursor t = ([], context t)

modifyContext :: (Context tag -> Maybe (Context tag)) -> Cursor tag -> Maybe (Cursor tag)
modifyContext f (cs, c) = ((cs,) . normalize) <$> f c

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
normalize :: Context a -> Context a
normalize (C tag (l:ls) []) = C tag ls [l]
normalize c = c

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

modifyUp :: (Context a -> Maybe (Context a)) -> Cursor a -> Maybe (Cursor a)
modifyUp f = up >=> modifyContext f >=> try down
modifyUp' :: (Context a -> Context a) -> Cursor a -> Maybe (Cursor a)
modifyUp' f = modifyUp (return . f)

next :: Cursor a -> Maybe (Cursor a)
next = modifyUp seekRight
insertNext :: a -> Cursor a -> Maybe (Cursor a)
insertNext tag = modifyUp (\c -> do
  c' <- try seekRight c
  return (c' `addChild` context tag))

previous :: Cursor a -> Maybe (Cursor a)
previous = modifyUp seekLeft
insertPrevious :: a -> Cursor a -> Maybe (Cursor a)
insertPrevious tag = modifyUp' (`addChild` context tag)

delete :: Cursor a -> Maybe (Cursor a)
delete = modifyUp removeChild

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
