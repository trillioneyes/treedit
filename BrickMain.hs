module BrickMain (main) where
import Lib
import System.Environment
import Treedit.IO
import qualified Treedit.Style as Style
import Brick hiding (Context)
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (borderStyleFromChar, unicode, BorderStyle)
import Graphics.Vty hiding (Cursor)
import Data.List

data TreeditState = Treedit {
    treeditPath :: Maybe FilePath,
    treeditCursor :: Cursor String
    }
    | Treeditor (Editor String TreeditWidget) TreeditState

data TreeditWidget = TextBox
    deriving (Eq, Ord, Show)

blankBorder :: BorderStyle
blankBorder = borderStyleFromChar ' '

drawTree :: (String -> Widget TreeditWidget) -> Tree (Style.Selectable String) -> Widget TreeditWidget
drawTree f (T (Style.Select tag) ts) = border $ f tag <=> children
    where children = foldl (<=>) (str "") (map indentStr ts)
          indentStr t = str "  " <+> drawTree f t
drawTree _ (T (Style.NoSelect tag) []) = str tag
drawTree f (T (Style.NoSelect tag) ts) = str tag <=> (str "  " <+> children)
    where children = foldl (<=>) (str "") (map (drawTree f) ts)

drawTreedit :: TreeditState -> (String -> Widget TreeditWidget) -> [Widget TreeditWidget]
drawTreedit (Treedit _ cur) f = [center $ drawTree f . stitch . addSelection $ cur]
drawTreedit (Treeditor ed st) _ = drawTreedit st drawEditor
    where drawEditor = (const $ hLimit width $ renderEditor renderContents True ed)
          renderContents (s:ss) = str s <=> renderContents ss
          renderContents [] = str ""
          width = maximum (map textWidth (getEditContents ed))

addSelection :: Cursor String -> Cursor (Style.Selectable String)
addSelection (cs, C tag ls rs) = let
        no :: [Context String] -> [Context (Style.Selectable String)]
        no = map $ fmap Style.NoSelect
    in (no cs, C (Style.Select tag) (no ls) (no rs))

charHandler :: Cursor String -> Char -> Cursor String
charHandler cur key = navigate key cur
    where navigate 'j' = try down
          navigate 'k' = try up
          navigate 'l' = try next
          navigate 'h' = try previous
          navigate 'J' = insertDown ""
          navigate 'K' = insertUp ""
          navigate 'L' = try (insertNext "")
          navigate 'H' = try (insertPrevious "")
          navigate _ = id

updateFromEditor :: Editor String TreeditWidget -> TreeditState -> TreeditState
updateFromEditor ed (Treedit path cur) = case getEditContents ed of
    [] -> Treedit path cur
    (name:_) -> Treedit path (rename name cur)

treeditor :: TreeditState -> TreeditState
treeditor s = Treeditor (editor TextBox (Just 1) (getFocusedName s)) s
    where getFocusedName (Treedit _ (_, C tag _ _)) = tag
          getFocusedName _ = ""

handleEvent :: TreeditState -> Event -> EventM TreeditWidget (Next TreeditState)
handleEvent (Treeditor ed st) (EvKey KEsc _) = continue st
handleEvent (Treeditor ed st) ev =
    do ed' <- handleEditorEvent ev ed
       continue (Treeditor ed' (updateFromEditor ed' st))
handleEvent st@Treedit{} (EvKey (KChar 'i') _) = continue $ treeditor st
handleEvent Treedit { treeditCursor = cur, treeditPath = path } (EvKey (KChar c) _)
    = continue $ Treedit {treeditCursor = charHandler cur c, treeditPath = path}
handleEvent s (EvKey KEsc _) = halt s
handleEvent s _ = continue s

app :: App TreeditState () TreeditWidget
app = App {
    appDraw = \st -> drawTreedit st str,
    appChooseCursor = \_ -> showCursorNamed TextBox,
    appHandleEvent = (\ s (VtyEvent e) -> handleEvent s e),
    appStartEvent = return,
    appAttrMap = const (attrMap defAttr [(editFocusedAttr, white `on` blue)])
    }

main :: IO ()
main = do
    (path:_) <- getArgs
    Just t <- readTree path
    defaultMain app (Treedit Nothing (unstitch t)) >> return ()
