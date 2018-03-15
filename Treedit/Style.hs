module Treedit.Style where
import Data.List

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
padV h x = take h (renderToLines x ++ repeat (replicate (width x) ' '))

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

type Style = String -> [Layout] -> Layout

delimitedList' :: Layout -> Layout -> Layout -> [Layout] -> Layout
delimitedList' open close sep xs = hcat [open, hcat (intersperse sep xs), close]

delimitedList :: String -> String -> String -> [Layout] -> Layout
delimitedList open close sep =
  delimitedList' (lit open) (lit close) (lit sep)

basic :: Style
basic text [] = hcat [lit "'", lit text, lit "'"]
basic tag xs@(_:_) =
  vcat [hcat [basic tag [], lit "("],
        hcat [lit "  ", vcat xs],
        lit ")"]

binOp :: Style
binOp op xs@(_:_:_) = hcat (intersperse (hcat [lit " ", lit op, lit " "]) xs)
binOp op xs = basic op xs

functionCall :: Style
functionCall f [] = lit f
functionCall f xs
  | width (hcat xs) <= 35 = hcat [
      lit f, delimitedList "(" ")" ", " xs
    ]
  | otherwise = basic f xs

selected :: Style -> Style
selected sty tag xs = let
  body = sty tag xs
  in hcat [body, vcat (replicate (height body) (lit "!!"))]