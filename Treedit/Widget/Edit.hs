module Treedit.Widget.Edit(handleEvent) where
import Brick.Widgets.Edit
import Graphics.Vty(Event(..), Key(..), Modifier(..))
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Data.Text.Zipper.Generic as Z

-- HACK: this function should be exported by Brick.Widgets.Edit. Since it isn't,
-- we define it ourselves.
handleEvent :: (Eq t, Monoid t) => Event -> Editor t n -> Editor t n
handleEvent e ed =
    let f = case e of
               EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
               EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
               EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
               EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
               EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
               EvKey KEnter [] -> Z.breakLine
               EvKey KDel [] -> Z.deleteChar
               EvKey (KChar c) [] | c /= '\t' -> Z.insertChar c
               EvKey KUp [] -> Z.moveUp
               EvKey KDown [] -> Z.moveDown
               EvKey KLeft [] -> Z.moveLeft
               EvKey KRight [] -> Z.moveRight
               EvKey KBS [] -> Z.deletePrevChar
               _ -> id
    in applyEdit f ed
