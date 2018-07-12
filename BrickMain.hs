{-# LANGUAGE OverloadedStrings #-}
module BrickMain (main) where
import Lib
import System.Environment
import Treedit.IO
import Treedit.Widget
import Brick(EventM, Next, BrickEvent(..), str, attrMap, on)
import Brick.Widgets.Center(center)
import Brick.Widgets.Edit(editFocusedAttr)
import Brick.Main
import Graphics.Vty hiding(Cursor)
import Data.List
import Data.Maybe

data TreeditState = Treedit {
    treeditPath :: Maybe FilePath,
    treeditCursor :: Cursor AnnString
    }

drawTreedit :: TreeditState -> [Widget]
drawTreedit st =
    [center . treeViewport . drawCursor . treeditCursor $ st]

charHandler :: Cursor AnnString -> Char -> Cursor AnnString
charHandler cur key = navigate key cur
    where navigate 'j' = try down
          navigate 'k' = try up
          navigate 'l' = try next
          navigate 'h' = try previous
          navigate 'J' = insertDown newEditor
          navigate 'K' = insertUp newEditor
          navigate 'L' = try (insertNext newEditor)
          navigate 'H' = try (insertPrevious newEditor)
          navigate 'i' = edit
          navigate 'x' = try Lib.delete
          navigate _ = id

handleNavigate :: Event -> Cursor AnnString -> EventM Name (Next (Cursor AnnString))
handleNavigate (EvKey (KChar c) _)  cur = continue $ charHandler cur c
handleNavigate (EvKey KEsc _) cur = halt cur
handleNavigate _ cur = continue cur

handleEvent :: TreeditState -> Event -> EventM Name (Next TreeditState)
handleEvent Treedit{ treeditPath = p, treeditCursor = cur } ev =
    case handleTagEdit ev cur of
        Just next -> continue . Treedit p $ next
        Nothing -> fmap (Treedit p) <$> handleNavigate ev cur

app :: App TreeditState () Name
app = App {
    appDraw = drawTreedit,
    appChooseCursor = showFirstCursor,
    appHandleEvent = (\ s (VtyEvent e) -> handleEvent s e),
    appStartEvent = return,
    appAttrMap = const (attrMap defAttr [(editFocusedAttr, black `on` white),
                                         ("red", brightRed `on` black),
                                         ("orange", brightYellow `on` black),
                                         ("yellow", brightBlue `on` black),
                                         ("green", brightCyan `on` black),
                                         ("blue", brightGreen `on` black),
                                         ("purple", brightMagenta `on` black)])
    }

processArgs :: [String] -> IO TreeditState
processArgs (path:_) = let emptyTree = T "" []
    in Treedit (Just path) . annotate . unstitch . fromMaybe emptyTree <$> readTree path
processArgs [] = return . Treedit Nothing . annotate . unstitch $ T "" []

main :: IO TreeditState
main = getArgs >>= processArgs >>= defaultMain app
