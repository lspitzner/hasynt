{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where



import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Language.Haskell.Hasynt
import Control.Concurrent.MVar
import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad ( when )



main :: IO ()
main = do
  _ <- initGUI
  builder <- builderNew
  builderAddFromFile builder "src-hasynt-gui/window.glade"
  window         <- builderGetObject builder castToWindow "window1"
  buttonRefresh  <- builderGetObject builder castToButton "buttonRefresh"
  textviewInput  <- builderGetObject builder castToTextView "textviewInput"
  textviewOutput <- builderGetObject builder castToTextView "textviewOutput"
  entryStatus    <- builderGetObject builder castToEntry    "entryStatus"
  checkbuttonTypeParen  <- builderGetObject builder castToCheckButton "checkbuttonTypeParen"
  checkbuttonValueParen <- builderGetObject builder castToCheckButton "checkbuttonValueParen"
  checkbuttonBraces     <- builderGetObject builder castToCheckButton "checkbuttonBraces"
  inputBuffer <- textViewGetBuffer textviewInput
  outputBuffer <- textViewGetBuffer textviewOutput
  mVarUpdate <- newMVar (0 :: Int)

  let
    updateOutput :: IO ()
    updateOutput = do
      s <- textBufferGetStartIter inputBuffer
      e <- textBufferGetEndIter   inputBuffer
      input <- textBufferGetText inputBuffer s e False
      let parsed = parse input
      let
        removeOldMark = do
          oldMarkM <- textBufferGetMark inputBuffer (T.pack "error")
          case oldMarkM of
            Nothing      -> return ()
            Just oldMark -> do
              textMarkSetVisible oldMark False
              textBufferDeleteMark inputBuffer oldMark
              removeOldMark
      case parsed of
        Left (l, c, err) -> do
          textBufferSetText outputBuffer ""
          removeOldMark
          iter <- textBufferGetIterAtLineOffset inputBuffer l (c-1)
          mark <- textBufferCreateMark inputBuffer (Just $ T.pack $ "error") iter True
          textMarkSetVisible mark True
          entrySetText entryStatus (show l ++ " " ++ err)
          return ()
        Right m -> do
          removeOldMark
          typeParen <- toggleButtonGetActive checkbuttonTypeParen
          valueParen <- toggleButtonGetActive checkbuttonValueParen
          braces <- toggleButtonGetActive checkbuttonBraces
          let output = prettyPrint braces
                     $ (if typeParen then addParensType else id)
                     $ (if valueParen then addParensValue else id)
                     $ m
          textBufferSetText outputBuffer output
          entrySetText entryStatus "success"
  let
    updateThread :: Int -> IO ()
    updateThread i = do
      threadDelay 1000000
      iNow <- readMVar mVarUpdate
      when (i==iNow) $ postGUIAsync updateOutput

  _ <- on buttonRefresh buttonActivated updateOutput
  _ <- on checkbuttonTypeParen  toggled updateOutput
  _ <- on checkbuttonValueParen toggled updateOutput
  _ <- on checkbuttonBraces     toggled updateOutput
  _ <- on inputBuffer bufferChanged $ do
    s <- modifyMVar mVarUpdate (\s -> return (s+1, s+1))
    _ <- forkIO $ updateThread s
    return ()
    -- putStrLn $ "changed"
  _ <- on window objectDestroy $ do
    -- widgetDestroy window
    mainQuit
  widgetShowAll window
  mainGUI
