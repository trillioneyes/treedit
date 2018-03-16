module Treedit.Style where
import Data.List
import Data.Maybe
import Control.Applicative
import Lib

type Rules = (FallbackRule, [RuleClause])
data RuleClause = Clause String Rule
type Rule = String -> [Layout] -> Maybe Layout
type FallbackRule = String -> [Layout] -> Layout

-- | Create a clause that doesn't render its display code.
invisible :: String -> ([Layout] -> Maybe Layout) -> RuleClause
invisible displayCode f = Clause displayCode (\_ subs -> f subs)

render :: Rules -> Tree (Selectable String) -> String
render rules = unlines . renderToLines . applyStyle rules

pseudoPy :: Rules
pseudoPy = (justTag, [
    Clause "BIN_OP" binOp,
    Clause "CALL" functionCall,
    Clause "DATA" rawData
  ])

simple :: Rules
simple = (explicit, [])

clause :: RuleClause -> Rule
clause (Clause clauseName f) displayCode ts
  | displayCode == clauseName = f displayCode ts
  | otherwise = Nothing

applyStyle :: Rules -> Tree (Selectable String) -> Layout
applyStyle def (T (Select tag) ts) =
  box $ applyStyle def (T (NoSelect tag) ts)
applyStyle def@(fallback, clauses) (T (NoSelect tag) ts) =
  fromMaybe basicLayout $ foldl (<|>) empty attempts where
    attempts :: [Maybe Layout]
    attempts = map (`clause` tag) clauses <*> [map (applyStyle def) ts]
    basicLayout = fallback tag . map (applyStyle def) $ ts

data Layout =
  Lit (Int, Int) String |
  VCat (Int, Int) Layout Layout |
  HCat (Int, Int) Layout Layout
 deriving Show

explicit :: FallbackRule
explicit displayCode [] = explicit displayCode [lit "(())"]
explicit displayCode (x:xs) =
 vcat [hcat [lit displayCode, lit ":"],
 hcat [lit "  ", vcat (x:xs)]]

justTag :: FallbackRule
justTag tag _ = lit tag

binOp :: Rule
binOp _ (op:xs@(_:_:_)) =
 Just $ hcat (intersperse (hcat [lit " ", op, lit " "]) xs)
binOp _ _ = Nothing

functionCall :: Rule
functionCall _ (f:args) = Just $ cat [header, body, footer] where
 cat = if width (hcat args) <= 35 then hcat else vcat
 header = hcat [f, lit "("]
 body = cat (intersperse (lit ", ") args)
 footer = lit ")"
functionCall _ _ = Nothing

rawData :: Rule
rawData _ [dat] = Just dat
rawData _ _ = Nothing

data Selectable a = Select a | NoSelect a
getSelectable :: Selectable a -> a
getSelectable (Select a) = a
getSelectable (NoSelect a) = a

lit' :: String -> Layout
lit' s = Lit (length s, 1) s
lit :: String -> Layout
lit s | '\n' `elem` s = vcat . map lit' . lines $ s
      | otherwise = lit' s
vcat :: [Layout] -> Layout
vcat [] = lit' ""
vcat [only] = only
vcat (t:rest) = let
  rest' = vcat rest
  in VCat (max (width t) (width rest'), height t + height rest') t rest'
hcat :: [Layout] -> Layout
hcat [] = lit' ""
hcat [only] = only
hcat (l:rest) = let
  rest' = hcat rest
  in HCat (width l + width rest', max (height l) (height rest')) l rest'

size :: Layout -> (Int, Int)
size (Lit sz _) = sz
size (VCat sz _ _) = sz
size (HCat sz _ _) = sz
width :: Layout -> Int
width = fst . size
height :: Layout -> Int
height = snd . size

pad :: (Int, Int) -> [String] -> [String]
pad (w, h) ss = take h . map (take w) $ infinite where
  infiniteLine = repeat ' '
  infinite = map (++ infiniteLine) ss ++ repeat infiniteLine

padV :: Int -> Layout -> [String]
padV h x = let
  diff = h - height x
  preHeight = ceiling (fromIntegral diff / 2 :: Double)
  postHeight = floor (fromIntegral diff / 2 :: Double)
  prePad = replicate preHeight (replicate (width x) ' ')
  postPad = replicate postHeight (replicate (width x) ' ')
  in prePad ++ renderToLines x ++ postPad

padH :: Int -> Layout -> [String]
padH w x = map (take w) (zipWith (++) (renderToLines x) (replicate (height x) (repeat ' ')))

renderToLines :: Layout -> [String]
renderToLines (Lit _ s) = [s]
renderToLines (HCat (_, h) l r) =
  zipWith (++) (padV h l) (padV h r)
renderToLines (VCat (w, _) t b) =
  padH w t ++ padH w b

box :: Layout -> Layout
box x =
  hcat [verticalBarrier,
        vcat [horizontalBarrier, x, horizontalBarrier],
        verticalBarrier] where
  verticalBarrier = vcat [lit "*", vcat $ replicate (height x) (lit "|"), lit "*"]
  horizontalBarrier = hcat (replicate (width x) (lit "-"))
