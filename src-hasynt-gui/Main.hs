{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where



import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Language.Haskell.Hasynt



main :: IO ()
main = do
  _ <- initGUI
  builder <- builderNew
  builderAddFromFile builder "src-hasynt-gui/window.glade"
  window         <- builderGetObject builder castToWindow "window1"
  buttonRefresh  <- builderGetObject builder castToButton "buttonRefresh"
  textviewInput  <- builderGetObject builder castToTextView "textviewInput"
  textviewOutput <- builderGetObject builder castToTextView "textviewOutput"
  _ <- on buttonRefresh buttonActivated $ do
    putStrLn "pressed!"
    inputBuffer <- textViewGetBuffer textviewInput
    outputBuffer <- textViewGetBuffer textviewOutput
    s <- textBufferGetStartIter inputBuffer
    e <- textBufferGetEndIter   inputBuffer
    input <- textBufferGetText inputBuffer s e False
    let output = prettyPrint $ addParens $ parse input
    textBufferSetText outputBuffer output
  _ <- on window objectDestroy $ do
    -- widgetDestroy window
    mainQuit
  widgetShowAll window
  mainGUI
