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
processCursor (cs, C tag ls rs) = (map draw cs, C (drawHighlight tag) (map draw ls) (map draw rs))
    where draw' :: AnnString -> [Widget] -> Widget
          draw' (Static tag) ws = str tag <=> boxChildren ws
          draw' (Editing e) ws = drawEditor e <=> boxChildren ws
          draw = fmap draw'
          drawHighlight t@(Static tag) ws = visible . border $ draw' t ws
          drawHighlight (Editing e) ws = (visible . border $ drawEditor e) <=> boxChildren ws
          drawEditor :: Editor -> Widget
          drawEditor e | all ((==0) . textWidth) (BWE.getEditContents e) = withAttr BWE.editFocusedAttr (str " ")
                       | otherwise = let c = BWE.getEditContents e
                                        in hLimit (maximum (map textWidth c)) . vLimit (length c) $ BWE.renderEditor (vBox . map str) True e

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
