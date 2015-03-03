module Language.Haskell.Hasynt
  ( parse
  , prettyPrint
  , transformInfixOperators
  , addParensType
  , addParensValue
  )
where



import Language.Haskell.Exts.Annotated.Syntax ( Module
                                              , Type(..)
                                              , Exp(..)
                                              , Stmt(..) )
import Language.Haskell.Exts.SrcLoc ( SrcLoc(SrcLoc) )
import qualified Language.Haskell.Exts.Annotated as Annotated
import qualified Language.Haskell.Exts.Pretty as PP
import Data.Data
import Control.Applicative ( (<$>) )
import Language.Haskell.Exts.Extension ( Language(Haskell2010)
                                       , Extension(EnableExtension)
                                       , KnownExtension(ExplicitForAll)
                                       )
import Language.Haskell.Exts.Annotated.Fixity ( baseFixities
                                              , applyFixities )
import HIndent.Types ( NodeInfo(..) )
import HIndent.Comments ( annotateComments )
import Data.Maybe ( fromMaybe )
import HIndent.Pretty ( pretty )
import qualified HIndent as H
import Data.Text.Internal.Builder ( toLazyText )
import Data.Text.Internal.Lazy    ( foldrChunks )
import qualified Data.Text.Lazy as DTL
import Language.Haskell.Exts.Annotated.Simplify ( sModule )



parse :: String -> Either (Int, Int, String) (Module NodeInfo)
parse s = case Annotated.parseModuleWithComments parseMode s of
  Annotated.ParseOk (m, comments) ->
    Right $ snd $ annotateComments ( fromMaybe m
                                 $ applyFixities baseFixities m)
                                 comments
  Annotated.ParseFailed (SrcLoc _ l c) s -> Left (l, c, s)

parseMode = Annotated.ParseMode
  ""
  Haskell2010
  [EnableExtension ExplicitForAll]
  False
  True
  (Just baseFixities)


prettyPrint :: Either Bool H.Style -> Module NodeInfo -> String
prettyPrint (Right style) ast = DTL.unpack $ toLazyText $ H.prettyPrint parseMode style $ pretty ast
prettyPrint (Left braces) ast =
  PP.prettyPrintStyleMode
    (PP.Style PP.PageMode 70 0.5)
    (PP.PPHsMode 2 2 2 2 4 2 2
                 False
                 (if braces then PP.PPSemiColon else PP.PPOffsideRule)
                 False)
    (sModule $ nodeInfoSpan <$> ast)

transformInfixOperators :: Module NodeInfo -> Module NodeInfo
transformInfixOperators = undefined

emptyNodeInfo :: NodeInfo
emptyNodeInfo = NodeInfo ( Annotated.noInfoSpan
                         $ Annotated.mkSrcSpan Annotated.noLoc Annotated.noLoc) []

addParensType :: Module NodeInfo -> Module NodeInfo
addParensType = dataMorph (typeTransform False)
 where
  addParen :: NodeInfo -> Bool -> Type NodeInfo -> Type NodeInfo
  addParen l True  = TyParen l
  addParen _ False = id
  typeTransform :: Bool -> Type NodeInfo -> Type NodeInfo
  typeTransform p (TyForall
        l
        tyVarBind
        context
        ty)                             = addParen l p $ TyForall l tyVarBind context $ typeTransform True ty
  typeTransform p (TyFun   l ty1 ty2)     = addParen l p $ TyFun l (typeTransform True ty1) (typeTransform True ty2)
  typeTransform p (TyTuple l boxed tys)   = TyTuple l boxed $ typeTransform False <$> tys
  typeTransform p (TyList  l ty)          = TyList l $ typeTransform False ty
  typeTransform p (TyParArray l ty)       = addParen l p $ TyParArray l $ typeTransform False ty
  typeTransform p (TyApp   l ty1 ty2)     = addParen l p $ TyApp l (typeTransform True ty1) (typeTransform True ty2)
  typeTransform p (TyVar   l n)           = TyVar l n
  typeTransform p (TyCon   l n)           = TyCon l n
  typeTransform p (TyParen l ty)          = TyParen l $ typeTransform False ty
  typeTransform p (TyInfix l ty1 n ty2)   = addParen l p $ TyInfix l (typeTransform False ty1) n (typeTransform False ty2)
  typeTransform p (TyKind  l ty k)        = addParen l p $ TyKind l (typeTransform True ty) k
  typeTransform p (TyPromoted l promoted) = addParen l p $ TyPromoted l promoted
  typeTransform p (TyEquals l ty1 ty2)    = addParen l p $ TyEquals l (typeTransform False ty1) (typeTransform False ty2)
  typeTransform p (TySplice l s)          = addParen l p $ TySplice l s
  typeTransform p (TyBang l bangType ty)  = addParen l p $ TyBang l bangType $ typeTransform False ty

addParensValue :: Module NodeInfo -> Module NodeInfo
addParensValue = dataMorph (valueTransform False)
 where
  addParen :: NodeInfo -> Bool -> Exp NodeInfo -> Exp NodeInfo
  addParen l True  = Paren l
  addParen _ False = id
  valueTransform :: Bool -> Exp NodeInfo -> Exp NodeInfo
  valueTransform p (Var l n)                       = Var l n
  valueTransform p (IPVar l n)                     = IPVar l n
  valueTransform p (Con l n)                       = Con l n
  valueTransform p (Lit l t)                       = Lit l t
  valueTransform p (InfixApp l e1 op e2)           = addParen l p $ InfixApp l (valueTransform True e1) op (valueTransform True e2)
  valueTransform p (App l e1 e2)                   = addParen l p $ App l (valueTransform True e1) (valueTransform True e2)
  valueTransform p (NegApp l e)                    = addParen l p $ NegApp l $ valueTransform True e
  valueTransform p (Lambda v ps e)                 = addParen emptyNodeInfo p $ Lambda v ps $ valueTransform True e -- TODO: pattern (?)
  valueTransform p (Let l binds e)                 = addParen l p $ Let l binds $ valueTransform True e -- TODO: binds (?)
  valueTransform p (If l e1 e2 e3)                 = addParen l p $ If l (valueTransform True e1) (valueTransform True e2) (valueTransform True e3)
  valueTransform p (MultiIf l guardRhss)           = addParen l p $ MultiIf l guardRhss -- TODO
  valueTransform p (Case l e alts)                 = addParen l p $ Case l (valueTransform False e) alts -- TODO: alts
  valueTransform p (Do l stmts)                    = addParen l p $ Do l (statementTransform <$> stmts)
  valueTransform p (MDo l stmts)                   = addParen l p $ MDo l (statementTransform <$> stmts)
  valueTransform p (Tuple l boxed es)              = Tuple l boxed $ valueTransform False <$> es
  valueTransform p (TupleSection l boxed mExps)    = TupleSection l boxed $ fmap (valueTransform False) <$> mExps
  valueTransform p (List l exps)                   = List l $ valueTransform False <$> exps
  valueTransform p (ParArray l exps)               = ParArray l $ valueTransform False <$> exps
  valueTransform p (Paren l e)                     = Paren l $ valueTransform False e
  valueTransform p (LeftSection l e op)            = addParen l p $ LeftSection l e op -- TODO
  valueTransform p (RightSection l op e)           = addParen l p $ RightSection l op e -- TODO
  valueTransform p (RecConstr l n updates)         = addParen l p $ RecConstr l n updates -- TODO
  valueTransform p (RecUpdate l e updates)         = addParen l p $ RecUpdate l e updates -- TODO
  valueTransform p (EnumFrom l e)                  = addParen l p $ EnumFrom l e -- TODO
  valueTransform p (EnumFromTo l e1 e2)            = addParen l p $ EnumFromTo l e1 e2 -- TODO
  valueTransform p (EnumFromThen l e1 e2)          = addParen l p $ EnumFromThen l e1 e2 -- TODO
  valueTransform p (EnumFromThenTo l e1 e2 e3)     = addParen l p $ EnumFromThenTo l e1 e2 e3 -- TODO
  valueTransform p (ParArrayFromTo l e1 e2)        = addParen l p $ ParArrayFromTo l e1 e2 -- TODO
  valueTransform p (ParArrayFromThenTo l e1 e2 e3) = addParen l p $ ParArrayFromThenTo l e1 e2 e3 -- TODO
  valueTransform p (ListComp l e stmts)            = addParen l p $ ListComp l e stmts -- TODO
  valueTransform p (ParComp l e stmtss)            = addParen l p $ ParComp l  e stmtss -- TODO
  valueTransform p (ParArrayComp l e stmtss)       = addParen l p $ ParArrayComp l e stmtss -- TODO
  valueTransform _ (ExpTypeSig loc e ty)           = addParen emptyNodeInfo True $ ExpTypeSig loc (valueTransform True e) ty
  valueTransform p (VarQuote l n)                  = addParen l p $ VarQuote l n -- TODO
  valueTransform p (TypQuote l n)                  = addParen l p $ TypQuote l n -- TODO
  valueTransform p (BracketExp l b)                = addParen l p $ BracketExp l b -- TODO
  valueTransform p (SpliceExp l splice)            = addParen l p $ SpliceExp l splice -- TODO
  valueTransform p (QuasiQuote l str1 str2)        = addParen l p $ QuasiQuote l str1 str2 -- TODO
  valueTransform p (XTag loc n attrs mExp es)      = addParen emptyNodeInfo p $ XTag loc n attrs mExp es -- TODO
  valueTransform p (XETag loc n attrs mExp)        = addParen emptyNodeInfo p $ XETag loc n attrs mExp -- TODO
  valueTransform p (XPcdata l str)                 = addParen l p $ XPcdata l str -- TODO
  valueTransform p (XExpTag l e)                   = addParen l p $ XExpTag l e -- TODO
  valueTransform p (XChildTag loc es)              = addParen emptyNodeInfo p $ XChildTag loc es -- TODO
  valueTransform p (CorePragma l str e)            = addParen l p $ CorePragma l str e -- TODO
  valueTransform p (SCCPragma  l str e)            = addParen l p $ SCCPragma  l str e -- TODO
  valueTransform p (GenPragma  l str p1 p2 e)      = addParen l p $ GenPragma  l str p1 p2 e -- TODO
  valueTransform p (Proc loc pat e)                = addParen emptyNodeInfo p $ Proc loc pat e -- TODO
  valueTransform p (LeftArrApp      l e1 e2)       = addParen l p $ LeftArrApp      l e1 e2 -- TODO
  valueTransform p (RightArrApp     l e1 e2)       = addParen l p $ RightArrApp     l e1 e2 -- TODO
  valueTransform p (LeftArrHighApp  l e1 e2)       = addParen l p $ LeftArrHighApp  l e1 e2 -- TODO
  valueTransform p (RightArrHighApp l e1 e2)       = addParen l p $ RightArrHighApp l e1 e2 -- TODO
  valueTransform p (LCase l alts)                  = addParen l p $ LCase l alts -- TODO
  statementTransform :: Stmt NodeInfo -> Stmt NodeInfo
  statementTransform (Generator loc pat e) = Generator loc pat (valueTransform True e)
  statementTransform (Qualifier l e)       = Qualifier l (valueTransform True e)
  statementTransform (LetStmt l binds)     = LetStmt l $ dataMorph (valueTransform False) binds
  statementTransform (RecStmt l stmts)     = RecStmt l $ statementTransform <$> stmts

-- why this function is not in Data.Data, i cannot tell..
dataMorph :: (Data d, Typeable a) => (a -> a) -> d -> d
dataMorph f o = case cast o >>= cast . f of
  Nothing -> gmapT (dataMorph f) o;
  Just x  -> x
