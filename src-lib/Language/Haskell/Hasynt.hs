module Language.Haskell.Hasynt
  ( parse
  , prettyPrint
  , transformInfixOperators
  , addParens
  )
where



import Language.Haskell.Exts.Syntax ( Module )
import qualified Language.Haskell.Exts.Parser as P
import qualified Language.Haskell.Exts.Pretty as PP



parse :: String -> Either String Module
parse s = case P.parse s of
  P.ParseOk m       -> Right m
  P.ParseFailed l s -> Left $ show l ++ " " ++ s

prettyPrint :: Module -> String
prettyPrint = PP.prettyPrint

transformInfixOperators :: Module -> Module
transformInfixOperators = undefined

addParens :: Module -> Module
addParens = id
