module Language.Haskell.Hasynt
  ( parse
  , prettyPrint
  , transformInfixOperators
  , addParensType
  )
where



import Language.Haskell.Exts.Syntax ( Module, Type(..) )
import Language.Haskell.Exts.SrcLoc ( SrcLoc(SrcLoc) )
import qualified Language.Haskell.Exts.Parser as P
import qualified Language.Haskell.Exts.Pretty as PP
import Data.Data
import Control.Applicative ( (<$>) )



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

addParensType :: Module -> Module
addParensType m = dataMorph (typeTransform False) m
 where
  addParen :: Bool -> Type -> Type
  addParen True  = TyParen
  addParen False = id
  typeTransform :: Bool -> Type -> Type
  typeTransform p (TyForall
        tyVarBind
        context
        ty)                             = addParen p $ TyForall tyVarBind context $ typeTransform True ty
  typeTransform p (TyFun   ty1 ty2)     = addParen p $ TyFun (typeTransform True ty1) (typeTransform True ty2)
  typeTransform p (TyTuple boxed tys)   = TyTuple boxed $ typeTransform False <$> tys
  typeTransform p (TyList  ty)          = TyList $ typeTransform False ty
  typeTransform p (TyParArray ty)       = addParen p $ TyParArray $ typeTransform False ty
  typeTransform p (TyApp   ty1 ty2)     = addParen p $ TyApp (typeTransform True ty1) (typeTransform True ty2)
  typeTransform p (TyVar   n)           = TyVar n
  typeTransform p (TyCon   n)           = TyCon n
  typeTransform p (TyParen ty)          = TyParen $ typeTransform False ty
  typeTransform p (TyInfix ty1 n ty2)   = addParen p $ TyInfix (typeTransform False ty1) n (typeTransform False ty2)
  typeTransform p (TyKind  ty k)        = addParen p $ TyKind (typeTransform True ty) k
  typeTransform p (TyPromoted promoted) = addParen p $ TyPromoted promoted
  typeTransform p (TyEquals ty1 ty2)    = addParen p $ TyEquals (typeTransform False ty1) (typeTransform False ty2)
  typeTransform p (TySplice s)          = addParen p $ TySplice s
  typeTransform p (TyBang bangType ty)  = addParen p $ TyBang bangType $ typeTransform False ty

-- why this function is not in Data.Data, i cannot tell..
dataMorph :: (Data d, Typeable a) => (a -> a) -> d -> d
dataMorph f o = case cast o >>= cast . f of Nothing -> gmapT (dataMorph f) o; Just x  -> x
