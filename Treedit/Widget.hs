module Treedit.Widget(
    Widget(..), Name(..), AnnString, annotate, 
    newEditor, edit, drawCursor, handleTagEdit,
    treeViewport
) where
import Brick hiding (Widget, Context)
import qualified Brick
import qualified Brick.Widgets.Edit as BWE
import Graphics.Vty.Input.Events(Event(EvKey), Key(KEsc))
import Brick.Widgets.Border(border)
import TreeParser
import Lib(Tree(..), Cursor, Context(..), stitch, mapContext)
import Treedit.Widget.Edit

data Name = CursorBox | TreeView deriving (Show, Eq, Ord)
data Selected = Yes | No deriving Show
data AnnString = Static String | Editing Editor deriving Show
type Widget = Brick.Widget Name
type Editor = BWE.Editor String Name

annotate :: Cursor String -> Cursor AnnString
annotate (cs, c) = (map no cs, no c) where
    no :: Context String -> Context AnnString
    no = fmap Static

annStr :: AnnString -> Widget
annStr (Static tag) = str tag
annStr (Editing e) = let
       c = BWE.getEditContents e
       width = maximum (map textWidth c) + 1
       height = maximum [1, length c]
    in hLimit width . vLimit height $ BWE.renderEditor (vBox . map str) True e

newEditor :: AnnString
newEditor = Editing $ BWE.editor CursorBox Nothing ""

save :: Editor -> AnnString
save = Static . unlines . BWE.getEditContents

edit :: Cursor AnnString -> Cursor AnnString
edit = mapContext edit' where
    edit' :: Context AnnString -> Context AnnString
    edit' (C (Static tag) ls rs) = C (Editing (BWE.editor CursorBox Nothing tag)) ls rs
    edit' cur = cur

boxChildren :: [Widget] -> Widget
boxChildren = vBox . map (padLeft (Pad 4))

processCursor :: Cursor AnnString -> Cursor ([Widget] -> Widget)
processCursor (cs, C tag ls rs) = (map (fmap draw) cs, C (visible . border . draw tag) (map (fmap draw) ls) (map (fmap draw) rs))
    where draw :: AnnString -> [Widget] -> Widget
          draw (Static t) ws = str t <=> boxChildren ws
          draw t ws = annStr t <=> boxChildren ws

drawTree :: Tree ([Widget] -> Widget) -> Widget
drawTree (T f ts) = f (map drawTree ts)

drawCursor :: Cursor AnnString -> Widget
drawCursor = drawTree . stitch . processCursor

handleTagEdit :: Event -> Cursor AnnString -> Maybe (Cursor AnnString)
handleTagEdit (EvKey KEsc _) (cs, C (Editing ed) ls rs) =
    Just (cs, C (save ed) ls rs)
handleTagEdit ev (cs, C (Editing ed) ls rs) =
    Just (cs, C (Editing $ handleEvent ev ed) ls rs)
handleTagEdit _ _ = Nothing

treeViewport :: Widget -> Widget
treeViewport = viewport TreeView Both
