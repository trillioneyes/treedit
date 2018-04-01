module Treedit.Widget(
    Widget(..), Name(..), AnnString, annotate,
    newEditor, edit, drawCursor, handleTagEdit
) where
import Brick hiding (Widget, Context)
import qualified Brick
import qualified Brick.Widgets.Edit as BWE
import Graphics.Vty.Input.Events(Event(EvKey), Key(KEsc))
import Brick.Widgets.Border(border)
import TreeParser
import Lib(Tree(..), Cursor, Context(..), stitch, mapContext)
import Treedit.Widget.Edit

data Name = CursorBox deriving (Show, Eq, Ord)
data Selected = Yes | No deriving Show
data AnnString = Static Selected String | Editing Editor deriving Show
type Widget = Brick.Widget Name
type Editor = BWE.Editor String Name

selected :: AnnString -> Selected
selected (Static Yes _) = Yes
selected (Editing _) = Yes
selected _ = No

annotate :: Cursor String -> Cursor AnnString
annotate (cs, c) = (map no cs, no c) where
    no :: Context String -> Context AnnString
    no = fmap $ Static No

newEditor :: AnnString
newEditor = Editing $ BWE.editor CursorBox Nothing ""

save :: Editor -> AnnString
save = Static Yes . unlines . BWE.getEditContents

edit :: Cursor AnnString -> Cursor AnnString
edit = mapContext edit' where
    edit' :: Context AnnString -> Context AnnString
    edit' (C (Static _ tag) ls rs) = C (Editing (BWE.editor CursorBox Nothing tag)) ls rs
    edit' cur = cur

drawTree :: Tree AnnString -> Widget
drawTree (T (Static Yes tag) ts) =
    visible . border . drawTree $ T (Static No tag) ts
drawTree (T (Static No tag) ts) = str tag <=> children where
    children = hBox $ map (padLeftRight 2 . drawTree) ts
drawTree (T (Editing e) ts) = visible (border tag) <=> children where
    tag = BWE.renderEditor (vBox . map str) True e
    children = hBox . map (padLeft (Pad 2) . drawTree) $ ts

drawCursor :: Cursor AnnString -> Widget
drawCursor (cs, c) = drawTree . stitch $ (cs, select c)
    where select :: Context AnnString -> Context AnnString
          select (C (Static _ tag) ls rs) = C (Static Yes tag) ls rs
          select (C (Editing e) ls rs) = C (Editing e) ls rs

handleTagEdit :: Event -> Cursor AnnString -> Maybe (Cursor AnnString)
handleTagEvent (EvKey KEsc _) (cs, C (Editing ed) ls rs) =
    Just (cs, C (save ed) ls rs)
handleTagEdit ev (cs, C (Editing ed) ls rs) =
    Just (cs, C (Editing $ handleEvent ev ed) ls rs)
handleTagEdit _ _ = Nothing
