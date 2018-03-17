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

single :: (Layout -> Layout) -> [Layout] -> Maybe Layout
single f [x] = Just $ f x
single _ _ = Nothing

render :: Rules -> Tree (Selectable String) -> String
render rules = unlines . renderToLines . applyStyle rules

pseudoPy :: Rules
pseudoPy = (justTag, [
    Clause "BIN_OP" binOp,
    Clause "CALL" functionCall,
    invisible "ARRAY" (Just . array),
    invisible "IF" pyIfStatement,
    invisible "PROTOTYPE" pyPrototype,
    invisible "BLOCK" (Just . vcat),
    invisible "ASSIGN" pyAssign,
    invisible "STRING_LITERAL" (single quotes),
    invisible "RETURN" (single returnStmt),
    invisible "ATTRIBUTE" dottedPath,
    invisible "VAR" (single var),
    invisible "ARGS" (Just . parens . hcat . commas)
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
explicit displayCode [] = lit displayCode
explicit displayCode (x:xs) =
 vcat [hcat [lit displayCode, lit ":"],
 hcat [lit "  ", vcat (x:xs)]]

justTag :: FallbackRule
justTag tag [] = lit tag
justTag tag (x:xs) = explicit tag (x:xs)

commas :: [Layout] -> [Layout]
commas = intersperse (lit ", ")

parens :: Layout -> Layout
parens x = hcat [lit "(", x, lit ")"]

indent :: Layout -> Layout
indent x = hcat [lit "  ", x]

array :: [Layout] -> Layout
array elems = fit [lit "[", fit' elems, lit "]"] where
  fit = fitCat 4
  fit' = fitCat' 80

fitCat :: Int -> [Layout] -> Layout
fitCat maxHeight xs | height (hcat xs) <= maxHeight = hcat xs
                    | otherwise = vcat xs

fitCat' :: Int -> [Layout] -> Layout
fitCat' maxWidth xs | width (hcat xs) <= maxWidth = hcat (commas xs)
                    | otherwise = indent $ vcat xs

binOp :: Rule
binOp _ (op:xs@(_:_:_)) =
 Just $ hcat (intersperse (hcat [lit " ", op, lit " "]) xs)
binOp _ _ = Nothing

functionCall :: Rule
functionCall _ (f:args) = Just $ cat [header, body, footer] where
 cat = if width (hcat args) <= 80 then hcat else vcat
 header = hcat [f, lit "("]
 body = cat (intersperse (lit ", ") args)
 footer = lit ")"
functionCall _ _ = Nothing

pyIfStatement :: [Layout] -> Maybe Layout
pyIfStatement [cond, ifThen] = Just $ vcat [
    hcat [lit "if ", cond, lit ":"],
    hcat [lit "  ", ifThen]
  ]
pyIfStatement [cond, ifThen, ifElse] = Just $ vcat [
    hcat [lit "if ", cond, lit ":"],
    hcat [lit "  ", ifThen],
    hcat [lit "else:"],
    hcat [lit "  ", ifElse]
  ]
pyIfStatement _ = Nothing

pyPrototype :: [Layout] -> Maybe Layout
pyPrototype [fName, fArgs, body] = Just $ vcat [
    hcat [lit "def ", fName, fArgs, lit ":"],
    indent body
  ]
pyPrototype _ = Nothing

pyAssign :: [Layout] -> Maybe Layout
pyAssign [name, value] = Just $ hcat [name, lit " = ", value]
pyAssign _ = Nothing

returnStmt :: Layout -> Layout
returnStmt x = hcat [lit "return ", x]

dottedPath :: [Layout] -> Maybe Layout
dottedPath xs@(_:_:_) = Just $ hcat (intersperse (lit ".") xs)
dottedPath _ = Nothing

quotes :: Layout -> Layout
quotes x = hcat [lit "\"", x, lit "\""]

var :: Layout -> Layout
var x = x

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
