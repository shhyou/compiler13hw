module Language.BLang.Homework.Homework4 (semanticCheck) where

import Language.BLang.Error (CompileError, errorAt)
import Language.BLang.FrontEnd.ParsedAST
import Language.BLang.FrontEnd.LexToken

import Data.Maybe (maybe)

type EWriter = Writer [CompileError]

-- types == [] -> start of new scope
-- types == [a] -> scalar
-- types == returnType : (args || [TVoid])
data Scope = Scope { identifier :: String,
                     types :: [Type],
                     source :: ParseTree }

openNewFuncScope = [Scope ".Func" [] (NonTerminal [])]
openNewBlockScope = [Scope ".Block" [] (NonTerminal [])]

getType :: [Scope] -> String -> Maybe [Type]
getType scope id' = find ((== id') . identifier) scope >>= (map check . types)
  where
    check TPtr itype = getType itype >>= TPtr
    check TArray aststmts itype = getType itype >>= (TArray aststmts)
    check TCustom innerid = getType innerid
    check nativeType = Just nativeType

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

    innerScope = argsScopes ++ openNewFuncScope ++ [funcScope] `scopeAddedTo` outerScope


checkReturnType :: Type -> Type -> Bool
checkReturnType TInt TInt = True
checkReturnType TInt TFloat = True
checkReturnType TInt _ = False
checkReturnType TFloat TInt = True
checkReturnType TFloat TFloat = True
checkReturnType TFloat _ = False
checkReturnType TVoid TVoid = True
checkReturnType TVoid _ = False
checkReturnType TChar TChar = True
checkReturnType TChar _ = False
checkReturnType (TPtr x) (TPtr y) = checkReturnType x y
checkReturnType (TPtr _) _ = False
checkReturnType (TArray _ _) _ = False -- TODOTODOTODOTODOTODO
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
    checkVarDecl scope' (term, (id', type', _)) = return [Scope id' type' term]


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
  let newscope = return scope'
  if isUnaryOp op
    then checkStmtType newscope (stmts !! 0)
    else checkStmtType newscope (stmts !! 0) >> checkStmtType newscope (stmts !! 1)


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
tellArrayToScalar parseTree a b =
  tellError parseTree ("Array " ++ a ++ " passed to scalar parameter " ++ b ++ ".")
tellScalarToArray parseTree a b =
  tellError parseTree ("Scalar " ++ a ++ " passed to array parameter " ++ b ++ ".")
checkStmtType scope (NonTerminal parseTrees, Ap stmt stmts) = undefined


checkStmtType scope (NonTerminal parseTrees, If condStmt thenStmt maybeElseStmt) = do
  scope' <- scope
  let newscope = return scope'
  checkStmtType newscope condStmt
  checkStmtType newscope thenStmt
  maybe (return TVoid) (checkStmtType newscope) maybeElseStmt


typeEqual TInt TInt = True
typeEqual TFloat TFloat = True
typeEqual TVoid TVoid = True
typeEqual TChar TChar = True
typeEqual (TPtr x) (TPtr y) = x `typeEqual` y
typeEqual (TArray xs x) (TArray ys y) = (length xs == length ys) && x `typeEqual` y
typeEqual TCustom TCustom = undefined
typeEqual _ _ = False

tellIncompatibleReturnType parseTree = tellError parseTree "Incompatible return type."
checkStmtType scope (NonTerminal parseTrees, Return maybeStmt) = do
  scope' <- scope
  let returnType = getFuncReturnType scope'
  if maybe TVoid it maybeStmt `typeEqual` returnType
    then return TVoid
    else tellIncompatibleReturnType (parseTrees !! 0) >> return TVoid


tellUndeclaredName parseTree id' = tellError parseTree ("ID " ++ id' ++ " undeclared.")
checkStmtType scope (Terminal parseTree, Identifier id') = do
  scope' <- scope
  case getType scope' id' of
    Nothing -> tellUndeclaredName parseTree id' >> return TVoid
    Just x -> return x


checkStmtType scope (Terminal parseTree, LiteralVal literal) = scope >> case literal of
  IntLiteral _ -> return TInt
  FloatLiteral _ -> return TFloat
  StringLiteral _ -> return (TPtr TChar)


tellSubscriptNotInt parseTree = tellError parseTree "Array subscript is not an integer"
tellIncompatibleArrayDims parseTree = tellError parseTree "Incompatible array dimensions."
checkStmtType scope (NonTerminal parseTrees, ArrayRef arrStmt dimStmt) = undefined

checkStmtType scope (_, Nop) = return TVoid
