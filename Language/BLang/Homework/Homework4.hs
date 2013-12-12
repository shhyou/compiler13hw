module Language.BLang.Homework.Homework4 (semanticCheck) where

import Language.BLang.Error (CompileError, errorAt)
import Language.BLang.FrontEnd.ParsedAST
import Language.BLang.FrontEnd.LexToken

import Control.Monad.Writer
import Data.Maybe (maybe)
import Data.List (find)
import Control.Monad (liftM2)

type EWriter = Writer [CompileError]

-- types == [] -> start of new scope
-- types == [a] -> scalar
-- types == returnType : args -> func
data Scope = Scope { identifier :: String,
                     types :: [Type],
                     source :: ParseTree }

openNewFuncScope = [Scope ".Func" [] (NonTerminal [])]
openNewBlockScope = [Scope ".Block" [] (NonTerminal [])]

-- need to return whole scope
getVar :: [Scope] -> String -> Maybe Scope
getVar scope id' = find ((== id') . identifier) scope >>= liftToTypes check
  where
    liftToTypes f (Scope id' types' source') = (mapM f types') >>= (\x -> Scope id' x source')
    check TPtr itype = getType itype >>= TPtr
    check TArray aststmts itype = getType itype >>= (TArray aststmts)
    check TCustom innerid = getType innerid
    check nativeType = Just nativeType

getType = fmap types . getVar

notEndOfScope :: Scope -> Bool
notEndOfScope = not . null . types

getFuncReturnType :: [Scope] -> Type
getFuncReturnType = head . types . last . takeWhile ((/= ".Func") . identifier)

type EWScope = EWriter [Scope]

tellError :: (Token Line) -> String -> CompileError
tellError (NonTerminal xs) = tellError $ head xs
tellError (Terminal lineToken) = tell . errorAt (getTokenData lineToken)

tellVariableRedeclared parseTree id' = tellError parseTree ("ID " ++ id' ++ " redeclared.")
scopeAddedTo :: EWScope -> EWScope -> EWScope
scopeAddedTo x y = x >>= foldr folder y
  where folder scope ys = do
          ys' <- ys
          if notEndOfScope scope && scope `elem` (takeWhile notEndOfScope ys')
            then
            tellVariableRedeclared (source scope) (identifier scope) >> ys
            else
            return (scope : ys')

popScope :: EWScope -> EWScope
popScope scope = scope >>= (tail . dropWhile notEndOfScope)


foldWith :: EWScope -> b -> ([Scope] -> b -> EWScope) -> EWScope
foldWith oldEWScope input f = do
  oldScope <- oldEWScope
  (f oldScope input) `scopeAddedTo` (return oldScope)


semanticCheck :: (AST.ParseTree, AST.AST) -> [CompileError]
semanticCheck = fst . runWriter . checkAST (Writer (openNewBlockScope, []))

checkAST :: EWScope -> (AST.ParseTree, AST.AST) -> EWScope
checkAST scope (NonTerminal parseTrees, asttops) =
  foldl (foldWith checkTop) scope (zip parseTrees asttops)

checkTop :: EWScope -> (AST.ParseTree, AST.ASTTop) -> EWScope
checkTop scope (NonTerminal parseTrees, VarDeclList astdecls) =
  foldl (foldWith checkDecl) scope (zip parseTrees astdecls)

checkTop scope (NonTerminal parseTree, FuncDecl retType name args code) =
  popScope . fmap fst $ checkStmtType innerScope code
  where
    funcScope = Scope name (returnType func : map snd args) parseTree

    argsToScope (id', type') = Scope id' [type'] parseTree
    argsScopes = map param args

    innerScope = argsScopes ++ openNewFuncScope ++ [funcScope] `scopeAddedTo` funcScope


typeIsScalar TInt = True
typeIsScalar TFloat = True
typeIsScalar TChar = True
typeIsScalar _ = False -- TVoid should be here(?

typeIsArray = not . typeIsScalar


checkReturnType :: Type -> Type -> Bool
checkReturnType TInt TInt = True
checkReturnType TInt TFloat = True
checkReturnType TFloat TInt = True
checkReturnType TFloat TFloat = True
checkReturnType TVoid TVoid = True
checkReturnType TChar TChar = True
checkReturnType (TPtr x) (TPtr y) = checkReturnType x y
checkReturnType (TArray xs tx) (TArray ys ty) = length xs == length ys && checkReturnType tx ty
checkReturnType _ _ = False   -- no TCustoms


tellVariableUndeclared parseTree id' = tellError parseTree ("ID " ++ id' ++ " undeclared.")

checkDecl :: EWScope -> (AST.ParseTree, ASTDecl) -> EWScope
checkDecl scope (NonTerminal parseTrees, TypeDecl types) =
  foldl (foldWith checkTypeDecl) scope (zip parseTrees types)
  where
    checkTypeDecl scope' (term, (id', type')) = case getType scope' id' of
      Just _ -> return [Scope id' [TCustom type'] term]
      Nothing -> tellVariableUndeclared term id' >> return []

checkDecl scope (NonTerminal parseTrees, VarDecl types) =
  foldl (foldWith checkVarDecl) scope (zip parseTrees types)
  where
    checkVarDecl scope' (term, (id', type', _)) = do
      scope'' <- scope'
      case type' of
        TArray stmts type' -> foldl folder (return True) prprs >>= returnIfTrue
          where
            prprs = zip (init . tail $ term) stmts
            returnIfTrue bool = do
              bool' <- bool
              case bool' of
                True -> return [Scope id' type' term]
                False -> tellSubscriptNotInt scope' >> return []
            isTInt x = case x of { TInt -> True; _ -> False; }
            folder bool prpr = do
              bool' <- bool
              checkStmtType (return scope'') prpr >>= ((bool' && ) . isTInt)
        _ -> return [Scope id' type' term]


type EWType = EWriter Type

chainStmtChecks scope = foldl (\(a, b) -> a >> checkStmtType scope b) TVoid

checkStmtType :: EWScope -> (AST.ParseTree, ASTStmt) -> EWType
checkStmtType scope (NonTerminal parseTrees, Block decls stmts) =
  chainStmtChecks innerScope (zip (parseTrees !! 1) stmts)
  where
    innerScope = foldl (foldWith checkDecl) TVoid (zip (parseTrees !! 0) decls)


isUnaryOp Negate = True
isUnaryOp LNot = True
isUnaryOp _ = False

unaryReturnType Negate = it
unaryReturnType LNot = const TInt
binaryReturnType Plus = largerType
binaryReturnType Minus = largerType
binaryReturnType Times = largerType
binaryReturnType Divide = largerType
binaryReturnType LT = const (const TInt)
binaryReturnType GT = const (const TInt)
binaryReturnType LEQ = const (const TInt)
binaryReturnType GEQ = const (const TInt)
binaryReturnType EQ = const (const TInt)
binaryReturnType NEQ = const (const TInt)
binaryReturnType LOr = const (const TInt)
binaryReturnType LAnd = const (const TInt)
binaryReturnType Assign = const . it

largerType TVoid TVoid = TVoid
largerType TVoid _ = undefined
largerType _ TVoid = undefined
largerType TChar TChar = TChar
largerType x TChar = largerType x TInt
largerType TChar y = largerType TInt y
largerType TInt TInt = TInt
largerType TFloat _ = TFloat
largerType _ TFloat = TFloat
largerType _ _ = undefined

checkStmtType scope (NonTerminal parseTrees, Expr op stmts) = do
  scope' <- scope
  let checkTypeWithScope = checkStmtType (return scope')
  if isUnaryOp op
    then fmap unaryReturnType $ checkTypeWithScope (stmts !! 0)
    else liftM2 binaryReturnType
                (checkTypeWithScope (stmts !! 0))
                (checkTypeWithScope (stmts !! 1))


checkStmtType scope (NonTerminal parseTrees, For initStmts condStmts iterStmts codeStmt) = do
  scope' <- scope
  let newscope = return scope'
  chainStmtChecks newscope initStmts
  chainStmtChecks newscope condStmts
  chainStmtChecks newscope iterStmts
  checkStmtType newscope codeStmt


checkStmtType scope (NonTerminal parseTrees, While condStmts codeStmt) =
  checkStmtType scope (NonTerminal newParseTree, For [] condStmts [] codeStmt)
  where newParseTree = [NonTerminal [], parseTrees !! 0, NonTerminal [], parseTrees !! 1]


tellTooFewArgs parseTree id' = tellError parseTree ("too few arguments to function " ++ id' ++ ".")
tellTooManyArgs parseTree id' = tellError parseTree ("too many arguments to function " ++ id' ++ ".")
checkStmtType scope (NonTerminal parseTrees, Ap (Identifier id') stmts) = do
  scope' <- scope
  args <- mapM (checkStmtType (return scope')) stmts
  case getVar scope' id' of
    Nothing -> tellVariableUndeclared (head parseTrees) id' >> return TVoid
    Just (Scope _ types' (NonTerminal funcParseTrees)) ->
      if defArgsNum > givenArgsNum
      then tellTooFewArgs (NonTerminal parseTrees) id' >> return TVoid
      else if defArgsNum < givenArgsNum
           then tellTooManyArgs (NonTerminal parseTrees) id' >> return TVoid
           else mapM_ checkFuncArgType (zip givenArgsWithId defArgsWithId) >> return (head types)
      where
        defArgsNum = length types' - 1
        givenArgsNum = length stmts
        defArgsWithId = zip (tail funcParseTrees) (tail types')
        givenArgsWithId = zip (tail parseTrees) stmts
        checkFuncArgType' = uncurry checkFuncArgType


tellArrayToScalar parseTree a b =
  tellError parseTree ("Array " ++ a ++ " passed to scalar parameter " ++ b ++ ".")
tellScalarToArray parseTree a b =
  tellError parseTree ("Scalar " ++ a ++ " passed to array parameter " ++ b ++ ".")
checkFuncArgType :: (ParseTree, Type) -> (ParseTree, Type) -> WError ()
checkFuncArgType (parseTreeA, typeA) (parseTreeB, typeB) =
  case (typeIsScalar typeA, typeIsScalar typeB) of
    (True, False) -> tellArrayToScalar parseTreeA idA idB >> return ()
    (False, True) -> tellScalarToArray parseTreeA idA idB >> return ()
    (False, False) -> return ()
    (True, True) -> if getArrayDim typeA /= getArrayDim typeB
                    then tellIncompatibleArrayDims parseTreeA >> return ()
                    else return ()
  where
    idA = getTokenId idA
    idB = getTokenId idB
    getArrayDim (TPtr type') = 1 + getArrayDim type'
    getArrayDim (TArray shit type') = length shit + getArrayDim type'
    getArrayDim _ = 0


getTokenId :: (Token a) -> String
getTokenId (LiteralToken (IntLiteral int)  _ _) = show int
getTokenId (LiteralToken (FloatLiteral flt) _ _) = show flt
getTokenId (LiteralToken (StringLiteral str) _ _) = str
getTokenId (Identifier str _) = str


checkStmtType scope (NonTerminal parseTrees, If condStmt thenStmt maybeElseStmt) = do
  scope' <- scope
  let newscope = return scope'
  checkStmtType newscope condStmt
  checkStmtType newscope thenStmt
  maybe (return TVoid) (checkStmtType newscope) maybeElseStmt


tellIncompatibleReturnType parseTree = tellError parseTree "Incompatible return type."
checkStmtType scope (NonTerminal parseTrees, Return maybeStmt) = do
  scope' <- scope
  let returnType = getFuncReturnType scope'
  if checkReturnType (maybe TVoid it maybeStmt) returnType
    then return TVoid
    else tellIncompatibleReturnType (parseTrees !! 0) >> return TVoid


tellUndeclaredName parseTree id' = tellError parseTree ("ID " ++ id' ++ " undeclared.")
checkStmtType scope (Terminal parseTree, Identifier id') = do
  scope' <- scope
  case getType scope' id' of
    Nothing -> tellUndeclaredName parseTree id' >> return TVoid
    Just x -> return x


checkStmtType scope (Terminal parseTree, LiteralVal literal) = scope >> return case literal of
  IntLiteral _ -> TInt
  FloatLiteral _ -> TFloat
  StringLiteral _ -> TPtr TChar


tellSubscriptNotInt parseTree = tellError parseTree "Array subscript is not an integer"
tellIncompatibleArrayDims parseTree = tellError parseTree "Incompatible array dimensions."
checkStmtType scope (NonTerminal parseTrees, ArrayRef arrStmt dimStmt) = do
  scope' <- scope
  let checkStmtType' = checkStmtType (return scope')
  arrType <- checkStmtType' (parseTrees !! 0, arrStmt)
  dimType <- checkStmtType' (parseTrees !! 1, dimStmt)
  case (arrType, dimType) of
    (TPtr type', TInt) -> return type'
    (TArray stmts type', TInt) -> return (TArray (tail stmts) type')
    (_, TInt) -> tellIncompatibleArrayDims (NonTerminal parseTrees) >> return TVoid
    (_, _) -> tellSubscriptNotInt (NonTerminal parseTrees) >> return TVoid


checkStmtType scope (_, Nop) = return TVoid
