{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where



import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Language.Haskell.Hasynt
import Control.Concurrent.MVar
import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad  ( when, forM )
import HIndent        ( testAll )
import HIndent.Pretty ( pretty )
import qualified HIndent as H



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
  radioPPSimple1     <- builderGetObject builder castToRadioButton "radiobuttonPPS1"
  radioPPSimple2     <- builderGetObject builder castToRadioButton "radiobuttonPPS2"
  radioPPChrisDone   <- builderGetObject builder castToRadioButton "radiobuttonPPChrisDone"
  radioPPJohanTibell <- builderGetObject builder castToRadioButton "radiobuttonPPJohanTibell"
  radioPPFundamental <- builderGetObject builder castToRadioButton "radiobuttonPPFundamental"
  radioPPGibiansky   <- builderGetObject builder castToRadioButton "radiobuttonPPGibiansky"

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
          let transformed = (if typeParen then addParensType else id)
                          $ (if valueParen then addParensValue else id)
                          $ m
          s1 <- toggleButtonGetActive radioPPSimple1
          s2 <- toggleButtonGetActive radioPPSimple2
          p1 <- toggleButtonGetActive radioPPChrisDone
          p2 <- toggleButtonGetActive radioPPJohanTibell
          p3 <- toggleButtonGetActive radioPPFundamental
          p4 <- toggleButtonGetActive radioPPGibiansky
          let
            style = if s1 then Left False
               else if s2 then Left True
               else if p1 then Right H.chrisDone
               else if p2 then Right H.johanTibell
               else if p3 then Right H.fundamental
               else if p4 then Right H.gibiansky
                          else Left False
          let output = prettyPrint style transformed
          -- _ $ pretty transformed
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
  _ <- on radioPPSimple1        toggled updateOutput
  _ <- on radioPPSimple1        toggled updateOutput
  _ <- on radioPPChrisDone      toggled updateOutput
  _ <- on radioPPJohanTibell    toggled updateOutput
  _ <- on radioPPFundamental    toggled updateOutput
  _ <- on radioPPGibiansky      toggled updateOutput
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
