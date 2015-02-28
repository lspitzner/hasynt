module Language.Haskell.Hasynt
  ( parse
  , prettyPrint
  , transformInfixOperators
  , addParens
  )
where



import Language.Haskell.Exts.Syntax ( Module )
import Language.Haskell.Exts.SrcLoc ( SrcLoc(SrcLoc) )
import qualified Language.Haskell.Exts.Parser as P
import qualified Language.Haskell.Exts.Pretty as PP



parse :: String -> Either (Int, Int, String) Module
parse s = case P.parse s of
  P.ParseOk m       -> Right m
  P.ParseFailed (SrcLoc _ l c) s -> Left (l, c, s)

prettyPrint :: Bool -> Module -> String
prettyPrint braces = PP.prettyPrintStyleMode
  (PP.Style PP.PageMode 70 0.5)
  (PP.PPHsMode 2 2 2 2 4 2 2
               False
               (if braces then PP.PPSemiColon else PP.PPOffsideRule)
               False)

transformInfixOperators :: Module -> Module
transformInfixOperators = undefined

addParens :: Module -> Module
addParens = id
