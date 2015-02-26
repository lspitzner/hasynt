module Language.Haskell.Hasynt
  (
  )
where



import Language.Haskell.Exts.Syntax ( Module )



parse :: String -> Module
parse = undefined

prettyPrint :: Module -> String
prettyPrint = undefined

transformInfixOperators :: Module -> Module
transformInfixOperators = undefined

addParens :: Module -> Module
addParens = undefined
