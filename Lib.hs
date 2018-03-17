{-# LANGUAGE DeriveGeneric, TupleSections #-}
module Lib where
import Data.Serialize(Serialize)
import GHC.Generics(Generic)
import Control.Applicative
import Control.Monad
import Data.Maybe

try' :: Alternative f => (a -> f a) -> a -> f a
try' f a = f a <|> pure a
try :: (a -> Maybe a) -> a -> a
try f a = fromMaybe a (f a)

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

mapContext :: (Context tag -> Context tag) -> Cursor tag -> Cursor tag
mapContext f (cs, c) = (cs, f c)

modifyContext :: (Context tag -> Maybe (Context tag)) -> Cursor tag -> Maybe (Cursor tag)
modifyContext f (cs, c) = (cs,) <$> f c

children :: Context a -> [Context a]
children (C _ ls rs) = reverse ls ++ rs
label :: Context a -> a
label (C tag _ _) = tag
addChild :: Context a -> Context a -> Context a
addChild (C tag ls rs) new = C tag ls (new:rs)
removeChild :: Context a -> Maybe (Context a)
removeChild (C tag ls (_:rs)) = Just $ C tag ls rs
removeChild (C tag (_:ls) []) = Just $ C tag ls []
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
descend (C _ (l:_) []) = Just l
descend _ = Nothing
swap :: Context a -> Maybe (Context a)
swap (C tag (l:ls) (r:rs)) = Just $ C tag (r:ls) (l:rs)
swap _ = Nothing

up :: Cursor a -> Maybe (Cursor a)
up ([], _) = Nothing
up (c:cs, v) = Just (cs, addChild c v)
insertUp :: a -> Cursor a -> Cursor a
insertUp tag = mapContext (context tag `addChild`)
swapUp :: Cursor a -> Maybe (Cursor a)
swapUp (c:cs, t) = Just (t:cs, c)
swapUp _ = Nothing
moveUp :: Cursor a -> Maybe (Cursor a)
moveUp = swapUp >=> up

down :: Cursor a -> Maybe (Cursor a)
down (cs, t) = do
  t' <- descend t
  c <- removeChild t
  return (c:cs, t')
insertDown :: a -> Cursor a -> Cursor a
insertDown tag = fromJust . down . mapContext (`addChild` context tag)
swapDown :: Cursor a -> Maybe (Cursor a)
swapDown = down >=> moveUp
moveDown :: Cursor a -> Maybe (Cursor a)
moveDown = down >=> swapUp

modifyUp :: (Context a -> Maybe (Context a)) -> Cursor a -> Maybe (Cursor a)
modifyUp f = up >=> modifyContext f >=> try' down
modifyUp' :: (Context a -> Context a) -> Cursor a -> Maybe (Cursor a)
modifyUp' f = modifyUp (return . f)

next :: Cursor a -> Maybe (Cursor a)
next = up >=> modifyContext seekRight >=> down
insertNext :: a -> Cursor a -> Maybe (Cursor a)
insertNext tag = up >=> modifyContext seekRight >=> (Just . insertDown tag)
swapNext :: Cursor a -> Maybe (Cursor a)
swapNext = moveNext >=> previous
moveNext :: Cursor a -> Maybe (Cursor a)
moveNext = next >=> swapPrevious

previous :: Cursor a -> Maybe (Cursor a)
previous = modifyUp seekLeft
insertPrevious :: a -> Cursor a -> Maybe (Cursor a)
insertPrevious tag = up >=> (return . insertDown tag)
swapPrevious :: Cursor a -> Maybe (Cursor a)
swapPrevious = up >=> modifyContext swap >=> down
movePrevious :: Cursor a -> Maybe (Cursor a)
movePrevious = swapPrevious >=> previous

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
