module Language.BLang.Homework.Homework4 (semanticCheck) where

import Prelude hiding (LT, GT, EQ) -- conflicts with ParsedAST

import Language.BLang.Data
import Language.BLang.Error (CompileError, errorAt)
import Language.BLang.FrontEnd.ParsedAST
import qualified Language.BLang.FrontEnd.LexToken as LexToken

import Control.Monad.Writer
import Data.Maybe (maybe)
import Data.List (find)
import Data.Functor ((<$>))
import Control.Monad (liftM2)


-- Errors
type EWriter = Writer [CompileError]
type EWScope = EWriter [Scope]
type EWType = EWriter Type

tellError :: ParseTree -> String -> EWriter ()
tellError (NonTerminal xs) = tellError $ head xs
tellError (Terminal lineToken) = tell . (:[]) . errorAt (LexToken.getTokenData lineToken)

tellIdRedeclared parseTree id' = tellError parseTree ("ID " ++ id' ++ " redeclared.")
tellIdUndeclared parseTree id' = tellError parseTree ("ID " ++ id' ++ " undeclared.")
tellTooFewArgs parseTree id' = tellError parseTree ("too few arguments to function " ++ id' ++ ".")
tellTooManyArgs parseTree id' = tellError parseTree ("too many arguments to function " ++ id' ++ ".")

tellArrayToScalar parseTree a b =
  tellError parseTree ("Array " ++ a ++ " passed to scalar parameter " ++ b ++ ".")
tellScalarToArray parseTree a b =
  tellError parseTree ("Scalar " ++ a ++ " passed to array parameter " ++ b ++ ".")

tellIncompatibleReturnType parseTree = tellError parseTree "Incompatible return type."
tellSubscriptNotInt parseTree = tellError parseTree "Array subscript is not an integer"
tellIncompatibleArrayDims parseTree = tellError parseTree "Incompatible array dimensions."


-- Types
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

isUnaryOp Negate = True
isUnaryOp LNot = True
isUnaryOp _ = False

unaryReturnType Negate = id
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
binaryReturnType Assign = const . id

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

checkFuncArgType :: (ParseTree, Type) -> (ParseTree, Type) -> EWriter ()
checkFuncArgType (parseTreeA, typeA) (parseTreeB, typeB) =
  case (typeIsScalar typeA, typeIsScalar typeB) of
    (True, False) -> tellArrayToScalar parseTreeA idA idB >> return ()
    (False, True) -> tellScalarToArray parseTreeA idA idB >> return ()
    (False, False) -> return ()
    (True, True) -> if getArrayDim typeA /= getArrayDim typeB
                    then tellIncompatibleArrayDims parseTreeA >> return ()
                    else return ()
  where
    idA = getTokenId' parseTreeA
    idB = getTokenId' parseTreeB
    getArrayDim (TPtr type') = 1 + getArrayDim type'
    getArrayDim (TArray shit type') = length shit + getArrayDim type'
    getArrayDim _ = 0



-- typedefs: Scope ("-" ++ newName) [aliasedType] source
-- types == [] -> start of new scope
-- types == [a] -> scalar
-- types == returnType : args -> func
data Scope = Scope { identifier :: String,
                     types :: [Type],
                     source :: ParseTree }

emptyParseTree = NonTerminal []
openNewFuncScope = [Scope ".Func" [] emptyParseTree]
openNewBlockScope = [Scope ".Block" [] emptyParseTree]
fundamentalBlock = [Scope "-int" [TInt] emptyParseTree,
                    Scope "-float" [TFloat] emptyParseTree,
                    Scope "-char" [TChar] emptyParseTree,
                    Scope "-void" [TVoid] emptyParseTree,
                    Scope ".BOF" [] emptyParseTree]

getVar :: [Scope] -> String -> Maybe Scope
getVar scope id' = find ((== id') . identifier) scope

getTypes :: [Scope] -> String -> Maybe [Type]
getTypes scope' = liftM types . getVar scope'

getType :: [Scope] -> String -> Maybe Type
getType scope' type' = getTypes scope' type' >>= extractType
  where extractType xs = if length xs /= 1 then Nothing else Just (head xs)

getTypeOfType :: [Scope] -> Type -> Maybe Type
getTypeOfType scope' (TCustom x) = getType scope' x
getTypeOfType scope' (TArray xs type') = liftM (\x -> TArray xs x) $ getTypeOfType scope' type'
getTypeOfType scope' (TPtr type') = liftM TPtr $ getTypeOfType scope' type'
getTypeOfType _ x = Just x

getFuncReturnType :: [Scope] -> Type
getFuncReturnType = head . types . last . takeWhile ((/= ".Func") . identifier)

-- reduces added vars' type to ``canonical form''
scopeAddedTo :: EWScope -> EWScope -> EWScope
scopeAddedTo x y = x >>= foldr folder y
  where
    folder scope ys = do    -- scope :: Scope, ys :: Error [CompileError] [Scope]
      ys' <- ys
      let
        (Scope id' oldTypes source') = scope
        canonical (TPtr type') = TPtr <$> canonical type'
        canonical (TArray xs type') = TArray xs <$> canonical type'
        canonical (TCustom id') = getType ys' ("-" ++ id')
        canonical others = Just others
      case mapM canonical oldTypes of
        _ | null . types $ scope ->  -- iff. scope is start of function/block scope
            return (scope : ys')
          | identifier scope `elem` (map identifier $ takeWhile (not . null . types) ys') ->
            tellIdRedeclared (source scope) (identifier scope) >> (return ys')
        Just newTypes ->
          return (Scope id' newTypes source' : ys')
        Nothing ->
          return ys'

semanticCheck :: (ParseTree, AST) -> [CompileError]
semanticCheck = snd . runWriter . checkAST (writer (fundamentalBlock, []))


-- checkBlaBla are type EWScope -> (ParseTree, ASTBlaBla) -> EWScope
-- foldWith checkBlaBla passes the inputScope to checkBlaBlaBla and adds the result to inputScope
foldWith :: (EWScope -> b -> EWScope) -> EWScope -> b -> EWScope
foldWith f oldEWScope input = do
  oldScope <- oldEWScope
  (f (return oldScope) input) `scopeAddedTo` (return oldScope)

checkAST :: EWScope -> (ParseTree, AST) -> EWScope
checkAST scope (NonTerminal parseTrees, asttops) =
  foldl (foldWith checkTop) scope (zip parseTrees asttops)

checkTop :: EWScope -> (ParseTree, ASTTop) -> EWScope
checkTop scope (NonTerminal parseTrees, VarDeclList astdecls) =
  foldl (foldWith checkDecl) scope (zip parseTrees astdecls)

checkTop scope (NonTerminal parseTree, FuncDecl retType name args code) = do
  scope' <- scope
  maybe (tellIdUndeclared (NonTerminal parseTree) name >> scope) id $ do -- TODO: should tell exact id instead of funcName
    let
      parseTree' = (NonTerminal parseTree)
      argsToScope (id', type') = liftM (\x -> Scope id' [x] parseTree') (getTypeOfType scope' type')
    argsTypes <- mapM (getTypeOfType scope' . snd) args
    argsScopes <- mapM argsToScope args
    let
      funcScope = [Scope name (retType : argsTypes) parseTree']
      outerScope = (return funcScope) `scopeAddedTo` (return scope')
      innerScope = (return $ argsScopes ++ openNewFuncScope) `scopeAddedTo` outerScope
    return $ checkStmtType innerScope (parseTree !! 3, code) >> outerScope


checkDecl :: EWScope -> (ParseTree, ASTDecl) -> EWScope
checkDecl scope (NonTerminal parseTrees, TypeDecl types) =
  foldl (foldWith checkTypeDecl) scope (zip parseTrees types)
  where
    checkTypeDecl scope' (term, (id', type')) = do
      scope'' <- scope'
      case getTypes scope'' id' of
        Just _ -> return [Scope id' [type'] term]
        Nothing -> tellIdUndeclared term id' >> return []

checkDecl scope (NonTerminal parseTrees, VarDecl types) =
  foldl (foldWith checkVarDecl) scope (zip parseTrees types)
  where
    checkVarDecl scope' (term, (id', type', _)) = do
      scope'' <- scope'
      case type' of
        TArray stmts type' -> foldl folder (return True) prprs >>= returnIfTrue
          where
            (NonTerminal termStmts) = term
            prprs = zip (init . tail $ termStmts) stmts
            returnIfTrue bool =
              case bool of
                True -> return [Scope id' [type'] term]
                False -> tellSubscriptNotInt term >> return []
            isTInt x = case x of { TInt -> True; _ -> False; }
            folder bool prpr = do
              bool' <- bool
              prprType <- checkStmtType (return scope'') prpr
              return $ bool' && isTInt prprType
        _ -> return [Scope id' [type'] term]


chainStmtChecks scope = foldl (\a b -> a >> checkStmtType scope b) (return TVoid)

getTokenId :: (LexToken.Token a) -> String
getTokenId (LexToken.LiteralToken (LexToken.IntLiteral int)  _ _) = show int
getTokenId (LexToken.LiteralToken (LexToken.FloatLiteral flt) _ _) = show flt
getTokenId (LexToken.LiteralToken (LexToken.StringLiteral str) _ _) = str
getTokenId (LexToken.Identifier str _ _) = str

getTokenId' (Terminal lineToken) = getTokenId lineToken

unpackTree (NonTerminal xs) = xs

checkStmtType :: EWScope -> (ParseTree, ASTStmt) -> EWType
checkStmtType scope (NonTerminal parseTrees, Block decls stmts) = do
  scope' <- scope
  let
    (NonTerminal declParseTrees) = parseTrees !! 0
    innerScope = foldl (foldWith checkDecl) (return scope') (zip declParseTrees decls)
    (NonTerminal stmtParseTrees) = parseTrees !! 1
  chainStmtChecks innerScope (zip stmtParseTrees stmts)


checkStmtType scope (NonTerminal parseTrees, Expr op stmts) = do
  scope' <- scope
  let checkTypeWithScope = checkStmtType (return scope')
  if isUnaryOp op
    then unaryReturnType op <$> checkTypeWithScope (parseTrees !! 1, stmts !! 0)
    else liftM2 (binaryReturnType op)
                (checkTypeWithScope (parseTrees !! 1, stmts !! 0))
                (checkTypeWithScope (parseTrees !! 2, stmts !! 1))

checkStmtType scope (NonTerminal parseTrees, For initStmts condStmts iterStmts codeStmt) = do
  scope' <- scope
  let
    newscope = return scope'
  chainStmtChecks newscope $ zip (unpackTree (parseTrees !! 0)) initStmts
  chainStmtChecks newscope $ zip (unpackTree (parseTrees !! 1)) condStmts
  chainStmtChecks newscope $ zip (unpackTree (parseTrees !! 2)) iterStmts
  checkStmtType newscope (parseTrees !! 3, codeStmt)

checkStmtType scope (NonTerminal parseTrees, While condStmts codeStmt) =
  checkStmtType scope (NonTerminal newParseTree, For [] condStmts [] codeStmt)
  where newParseTree = [NonTerminal [], parseTrees !! 0, NonTerminal [], parseTrees !! 1]

checkStmtType scope (NonTerminal parseTrees, Ap (Identifier id') stmts) = do
  scope' <- scope
  args <- mapM (checkStmtType (return scope')) (zip (drop 2 parseTrees) stmts)
  case getVar scope' id' of
    Nothing -> tellIdUndeclared (parseTrees !! 1) id' >> return TVoid
    Just (Scope _ types' (NonTerminal funcParseTrees)) ->
      if defArgsNum > givenArgsNum
      then tellTooFewArgs (NonTerminal parseTrees) id' >> return TVoid
      else if defArgsNum < givenArgsNum
           then tellTooManyArgs (NonTerminal parseTrees) id' >> return TVoid
           else mapM_ checkFuncArgType' (zip givenArgsWithId defArgsWithId) >> return (head types')
      where
        defArgsNum = length types' - 1
        givenArgsNum = length stmts
        defArgsWithId = zip (tail funcParseTrees) (tail types')
        givenArgsParseTrees = unpackTree (parseTrees !! 1)
        givenArgsStmts = mapM (checkStmtType (return scope')) (zip givenArgsParseTrees stmts)
        givenArgsWithId = zip (tail parseTrees) undefined
        checkFuncArgType' = uncurry checkFuncArgType

checkStmtType scope (NonTerminal parseTrees, If condStmt thenStmt maybeElseStmt) = do
  scope' <- scope
  let newscope = return scope'
  checkStmtType newscope (parseTrees !! 0, condStmt)
  checkStmtType newscope (parseTrees !! 1, thenStmt)
  maybe (return TVoid) (\x -> checkStmtType newscope (parseTrees !! 2, x)) maybeElseStmt

checkStmtType scope (NonTerminal parseTrees, Return maybeStmt) = do
  scope' <- scope
  let
    returnType = getFuncReturnType scope'
    checkStmtType' x = checkStmtType (return scope') (parseTrees !! 1, x)
  actualReturnType <- maybe (return TVoid) checkStmtType' maybeStmt
  if checkReturnType actualReturnType returnType
    then return TVoid
    else tellIncompatibleReturnType (parseTrees !! 0) >> return TVoid

checkStmtType scope (Terminal parseTree, Identifier id') = do
  scope' <- scope
  case getType scope' id' of
    Nothing -> tellIdUndeclared (Terminal parseTree) id' >> return TVoid
    Just x -> return x

checkStmtType scope (Terminal parseTree, LiteralVal literal) = scope >> case literal of
  IntLiteral _ -> return TInt
  FloatLiteral _ -> return TFloat
  StringLiteral _ -> return (TPtr TChar)

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
