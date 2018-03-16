module Treedit.Style where
import Data.List
import Data.Maybe
import Control.Applicative
import Lib

data Layout =
  Lit (Int, Int) String |
  VCat (Int, Int) Layout Layout |
  HCat (Int, Int) Layout Layout
 deriving Show

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
  prePad = replicate (ceiling (realToFrac diff/2)) (replicate (width x) ' ')
  postPad = replicate (floor (realToFrac diff/2)) (replicate (width x) ' ')
  in prePad ++ renderToLines x ++ postPad

padH :: Int -> Layout -> [String]
padH w x = map (take w) (zipWith (++) (renderToLines x) (replicate (height x) (repeat ' ')))

renderToLines :: Layout -> [String]
renderToLines (Lit _ s) = [s]
renderToLines (HCat (_, h) l r) =
  zipWith (++) (padV h l) (padV h r)
renderToLines (VCat (w, _) t b) =
  padH w t ++ padH w b

render :: Layout -> String
render = unlines . renderToLines

type Style = Layout -> [Layout] -> Layout

delimitedList' :: Layout -> Layout -> Layout -> [Layout] -> Layout
delimitedList' open close sep xs = hcat [open, hcat (intersperse sep xs), close]

delimitedList :: String -> String -> String -> [Layout] -> Layout
delimitedList open close sep =
  delimitedList' (lit open) (lit close) (lit sep)

box :: Layout -> Layout
box x =
  hcat [verticalBarrier,
        vcat [horizontalBarrier, x, horizontalBarrier],
        verticalBarrier] where
  verticalBarrier = vcat [lit "*", vcat $ replicate (height x) (lit "|"), lit "*"]
  horizontalBarrier = hcat (replicate (width x) (lit "-"))

basic :: Style
basic text [] = text
basic tag xs@(_:_) =
  vcat [hcat [basic tag [], lit ":"],
        hcat [lit "  ", vcat xs]]

binOp :: Style
binOp op xs@(_:_:_) = hcat (intersperse (hcat [lit " ", op, lit " "]) xs)
binOp op xs = basic op xs

functionCall :: Style
functionCall f [] = f
functionCall f xs
  | width (hcat xs) <= 35 = hcat [
      f, delimitedList "(" ")" ", " xs
    ]
  | otherwise = basic f xs

selected :: Style -> Style
selected sty tag xs = let
  body = sty tag xs
  in box body

data Selectable a = Select a | NoSelect a
getSelectable :: Selectable a -> a
getSelectable (Select a) = a
getSelectable (NoSelect a) = a

type TreeLayoutRule = Tree (Selectable String) -> Layout

treeLayout :: Tree (Selectable String) -> Layout
treeLayout (T (Select tag) ts) =
  box . treeLayout $ T (NoSelect tag) ts
treeLayout (T (NoSelect tag) []) = lit tag
treeLayout (T (NoSelect "BIN_OP") (t:ts)) =
  binOp (treeLayout t) (map treeLayout ts)
treeLayout (T (NoSelect "CALL") (t:ts)) =
  functionCall (treeLayout t) (map treeLayout ts)
treeLayout (T _ (t:ts)) =
  basic (treeLayout t) (map treeLayout ts)

basicTreeLayout :: Tree (Selectable String) -> Layout
basicTreeLayout (T (Select tag) ts) = box . basicTreeLayout $ T (NoSelect tag) ts
basicTreeLayout (T (NoSelect tag) ts) = basic (lit tag) (map basicTreeLayout ts)

type RulesDefinition = (String -> [Layout] -> Layout, [RuleClause])

data RuleClause = Clause String (String -> [Layout] -> Maybe Layout)

clause :: RuleClause -> String -> [Layout] -> Maybe Layout
clause (Clause clauseName f) tag ts
  | tag == clauseName = f tag ts
  | otherwise = Nothing

definition :: RulesDefinition -> Tree (Selectable String) -> Layout
definition def (T (Select tag) ts) =
  box $ definition def (T (NoSelect tag) ts)
definition def@(fallback, clauses) (T (NoSelect tag) ts) =
  fromMaybe basicLayout $ foldl (<|>) empty attempts where
    attempts :: [Maybe Layout]
    attempts = map (`clause` tag) clauses <*> [map (definition def) ts]
    basicLayout = fallback tag . map (definition def) $ ts
