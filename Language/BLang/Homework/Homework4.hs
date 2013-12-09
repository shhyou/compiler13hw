module Language.BLang.Homework.Homework4 (semanticCheck) where

import Language.BLang.Error (CompileError)
import Language.BLang.FrontEnd.ParsedAST (ParseTree, AST)
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
    check TArray aststmts itype = getType itype >>= (TPtr aststmts)
    check TCustom innerid = getType innerid
    check nativeTypes = Just nativeTypes

notEndOfScope = Scope -> Bool
notEndOfScope = not . null . types

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

checkTop scope (NonTerminal parseTree, func) =
  popScope . checkStmt $ innerScope (funcCode func)
  where
    argTypes = map snd (funcArgs func)
    funcScope = Scope (funcName func) (returnType func : argTypes) parseTree

    argsToScope (id', type') = Scope id' [type'] parseTree
    argsScopes = map param (funcArgs func)

    innerScope = argsScopes ++ openNewScope ++ [funcScope] `scopeAddedTo` outerScope


tellVariableUndeclared parseTree id' = tellError parseTree 180 ("ID " ++ id' ++ " undeclared.")

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


checkFuncDecl scope = undefined
