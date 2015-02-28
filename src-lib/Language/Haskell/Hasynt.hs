module Language.Haskell.Hasynt
  ( parse
  , prettyPrint
  , transformInfixOperators
  , addParensType
  , addParensValue
  )
where



import Language.Haskell.Exts.Syntax ( Module
                                    , Type(..)
                                    , Exp(..) )
import Language.Haskell.Exts.SrcLoc ( SrcLoc(SrcLoc) )
import qualified Language.Haskell.Exts.Parser as P
import qualified Language.Haskell.Exts.Pretty as PP
import Data.Data
import Control.Applicative ( (<$>) )
import Language.Haskell.Exts.Extension ( Language(Haskell2010)
                                       , Extension(EnableExtension)
                                       , KnownExtension(ExplicitForAll)
                                       )
import Language.Haskell.Exts.Fixity    ( baseFixities )



parse :: String -> Either (Int, Int, String) Module
parse s = case P.parseWithMode parseMode s of
  P.ParseOk m       -> Right m
  P.ParseFailed (SrcLoc _ l c) s -> Left (l, c, s)
 where
  parseMode = P.ParseMode
    ""
    Haskell2010
    [EnableExtension ExplicitForAll]
    False
    True
    (Just baseFixities)


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
addParensType = dataMorph (typeTransform False)
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

addParensValue :: Module -> Module
addParensValue = dataMorph (valueTransform False)
 where
  addParen :: Bool -> Exp -> Exp
  addParen True  = Paren
  addParen False = id
  valueTransform :: Bool -> Exp -> Exp
  valueTransform p (Var n)                             = Var n
  valueTransform p (IPVar n)                           = IPVar n
  valueTransform p (Con n)                             = Con n
  valueTransform p (Lit l)                             = Lit l
  valueTransform p (InfixApp e1 op e2)                 = addParen p $ InfixApp (valueTransform True e1) op (valueTransform True e2)
  valueTransform p (App e1 e2)                         = addParen p $ App (valueTransform True e1) (valueTransform True e2)
  valueTransform p (NegApp e)                          = addParen p $ NegApp $ valueTransform True e
  valueTransform p (Lambda l ps e)                     = addParen p $ Lambda l ps $ valueTransform True e -- TODO: pattern (?)
  valueTransform p (Let binds e)                       = addParen p $ Let binds $ valueTransform True e -- TODO: binds (?)
  valueTransform p (If e1 e2 e3)                       = addParen p $ If (valueTransform True e1) (valueTransform True e2) (valueTransform True e3)
  valueTransform p (MultiIf guardRhss)                 = addParen p $ MultiIf guardRhss -- TODO
  valueTransform p (Case e alts)                       = addParen p $ Case (valueTransform False e) alts -- TODO: alts
  valueTransform p (Do stmts)                          = addParen p $ Do stmts -- TODO: stmts
  valueTransform p (MDo stmts)                         = addParen p $ MDo stmts -- TODO: stmts
  valueTransform p (Tuple boxed es)                    = Tuple boxed $ valueTransform False <$> es
  valueTransform p (TupleSection boxed mExps)          = TupleSection boxed $ fmap (valueTransform False) <$> mExps
  valueTransform p (List exps)                         = List $ valueTransform False <$> exps
  valueTransform p (ParArray exps)                     = ParArray $ valueTransform False <$> exps
  valueTransform p (Paren e)                           = Paren $ valueTransform False e
  valueTransform p (LeftSection e op)                  = addParen p $ LeftSection e op -- TODO
  valueTransform p (RightSection op e)                 = addParen p $ RightSection op e -- TODO
  valueTransform p (RecConstr n updates)               = addParen p $ RecConstr n updates -- TODO
  valueTransform p (RecUpdate e updates)               = addParen p $ RecUpdate e updates -- TODO
  valueTransform p (EnumFrom e)                        = addParen p $ EnumFrom e -- TODO
  valueTransform p (EnumFromTo e1 e2)                  = addParen p $ EnumFromTo e1 e2 -- TODO
  valueTransform p (EnumFromThen e1 e2)                = addParen p $ EnumFromThen e1 e2 -- TODO
  valueTransform p (EnumFromThenTo e1 e2 e3)           = addParen p $ EnumFromThenTo e1 e2 e3 -- TODO
  valueTransform p (ParArrayFromTo e1 e2)              = addParen p $ ParArrayFromTo e1 e2 -- TODO
  valueTransform p (ParArrayFromThenTo e1 e2 e3)       = addParen p $ ParArrayFromThenTo e1 e2 e3 -- TODO
  valueTransform p (ListComp e stmts)                  = addParen p $ ListComp e stmts -- TODO
  valueTransform p (ParComp  e stmtss)                 = addParen p $ ParComp  e stmtss -- TODO
  valueTransform p (ParArrayComp e stmtss)             = addParen p $ ParArrayComp e stmtss -- TODO
  valueTransform p (ExpTypeSig loc e ty)               = addParen p $ ExpTypeSig loc e ty -- TODO
  valueTransform p (VarQuote n)                        = addParen p $ VarQuote n -- TODO
  valueTransform p (TypQuote n)                        = addParen p $ TypQuote n -- TODO
  valueTransform p (BracketExp b)                      = addParen p $ BracketExp b -- TODO
  valueTransform p (SpliceExp splice)                  = addParen p $ SpliceExp splice -- TODO
  valueTransform p (QuasiQuote str1 str2)              = addParen p $ QuasiQuote str1 str2 -- TODO
  valueTransform p (XTag loc n attrs mExp es)          = addParen p $ XTag loc n attrs mExp es -- TODO
  valueTransform p (XETag loc n attrs mExp)            = addParen p $ XETag loc n attrs mExp -- TODO
  valueTransform p (XPcdata str)                       = addParen p $ XPcdata str -- TODO
  valueTransform p (XExpTag e)                         = addParen p $ XExpTag e -- TODO
  valueTransform p (XChildTag loc es)                  = addParen p $ XChildTag loc es -- TODO
  valueTransform p (CorePragma str e)                  = addParen p $ CorePragma str e -- TODO
  valueTransform p (SCCPragma  str e)                  = addParen p $ SCCPragma  str e -- TODO
  valueTransform p (GenPragma  str p1 p2 e)            = addParen p $ GenPragma  str p1 p2 e -- TODO
  valueTransform p (Proc loc pat e)                    = addParen p $ Proc loc pat e -- TODO
  valueTransform p (LeftArrApp      e1 e2)             = addParen p $ LeftArrApp      e1 e2 -- TODO
  valueTransform p (RightArrApp     e1 e2)             = addParen p $ RightArrApp     e1 e2 -- TODO
  valueTransform p (LeftArrHighApp  e1 e2)             = addParen p $ LeftArrHighApp  e1 e2 -- TODO
  valueTransform p (RightArrHighApp e1 e2)             = addParen p $ RightArrHighApp e1 e2 -- TODO
  valueTransform p (LCase alts)                        = addParen p $ LCase alts -- TODO

-- why this function is not in Data.Data, i cannot tell..
dataMorph :: (Data d, Typeable a) => (a -> a) -> d -> d
dataMorph f o = case cast o >>= cast . f of
  Nothing -> gmapT (dataMorph f) o;
  Just x  -> x
