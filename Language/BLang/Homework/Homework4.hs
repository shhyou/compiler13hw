module Language.BLang.Homework.Homework4 (semanticCheck) where

import Language.BLang.Error
import Language.BLang.FrontEnd.ParsedAST
import Language.BLang.FrontEnd.LexToken

type EWriter = Writer [CompileError]

-- types == [] -> start of new scope
-- types == [a] -> scalar
-- types == returnType : (args || [TVoid])
data Scope = Scope { identifier :: String,
                     types :: [Type],
                     source :: ParseTree }

openNewScope = [Scope "" [] (NonTerminal [])]

getType :: [Scope] -> String -> Maybe [Type]
getType scope id' = find ((== id') . identifier) scope >>= (map check . types)
  where
    check TPtr itype = getType itype >>= TPtr
    check TArray aststmts itype = getType itype >>= (TArray aststmts)
    check TCustom innerid = getType innerid
    check nativeTypes = Just nativeTypes

notEndOfScope = Scope -> Bool
notEndOfScope = not . null . types

getFuncReturnType :: [Scope] -> Maybe Type
getFuncReturnType = fmap (head . types) . listToMaybe . reverse . takeWhile notEndOfScope

type EWScope = EWriter [Scope]

tellError (NonTerminal xs) = tellError $ head xs
tellError (Terminal (_ line)) = tell . CompileError line

scopeAddedTo :: EWScope -> EWScope -> EWScope
scopeAddedTo x y = x >>= foldr folder y
  where folder scope ys = do
          ys' <- ys
          if scope `elem` (takeWhile notEndOfScope ys')
            then
            tellError (source scope) (180) ("ID " ++ (identifier scope) ++ " redeclared.") >> ys
            else
            return (scope : ys')

popScope :: EWScope -> EWScope
popScope scope = scope >>= (tail . dropWhile notEndOfScope)


foldWith :: EWScope -> b -> ([Scope] -> b -> EWScope) -> EWScope
foldWith oldEWScope input f = do
  oldScope <- oldEWScope
  (f oldScope input) `scopeAddedTo` (return oldScope)


semanticCheck :: (AST.ParseTree, AST.AST) -> [CompileError]
semanticCheck (NonTerminal parseTrees, asttops) =
  fst . runWriter $ foldl (foldWith checkTop) (Writer (openNewScope, [])) (zip parseTrees asttops)


checkTop :: EWScope -> (AST.ParseTree, AST.ASTTop) -> EWScope
checkTop scope (NonTerminal parseTrees, VarDeclList astdecls) =
  foldl (foldWith checkDecl) scope (zip parseTrees astdecls)

checkTop scope (NonTerminal parseTree, FuncDecl retType name args code) =
  popScope $ checkStmtType innerScope code
  where
    funcScope = Scope name (returnType func : map snd args) parseTree

    argsToScope (id', type') = Scope id' [type'] parseTree
    argsScopes = map param args

    innerScope = argsScopes ++ openNewScope ++ [funcScope] `scopeAddedTo` outerScope


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


type EWST = EWriter ([Scope], Type)

checkStmtType :: EWScope -> (AST.ParseTree, ASTStmt) -> EWST
checkStmtType scope (NonTerminal parseTrees, Block decls stmts) =
  popScope $ foldl (foldWith checkStmtType) innerScope (zip (parseTrees !! 0) stmts)
  where
    innerScope = foldl (foldWith checkDecl) scope (zip (parseTrees !! 1) decls)

checkStmtType scope (NonTerminal parseTrees, Expr op stmts) = undefined  -- some long shit


checkStmtType scope (NonTerminal parseTrees, For initStmts condStmts iterStmts codeStmt) = undefined

checkStmtType scope (NonTerminal parseTrees, While condStmts codeStmt) =
  checkStmtType scope (NonTerminal newParseTree, For [] condStmts [] codeStmt)
  where newParseTree = [NonTerminal [], parseTrees !! 0, NonTerminal [], parseTrees !! 1]

-- too few arguments to function <name>.
-- too many arguments to function <name>.
-- Array <name> passed to scalar parameter <name>.
-- Scalar <name> passed to array parameter <name>.
checkStmtType scope (NonTerminal parseTrees, Ap stmt stmts) = undefined
checkStmtType scope (NonTerminal parseTrees, If condStmt thenStmt maybeElseStmt) = undefined

-- Incompatible return type.
checkStmtType scope (NonTerminal parseTrees, Return maybeStmt) = undefined

-- ID <name> undeclared.
checkStmtType scope (NonTerminal parseTrees, Identifier id') = undefined

checkStmtType scope (NonTerminal parseTrees, LiteralVal literal) = undefined

-- Array subscript is not an integer
-- Incompatible array dimensions.
checkStmtType scope (NonTerminal parseTrees, ArrayRef arrStmt dimStmt) = undefined
checkStmtType scope (_, Nop) = scope
