module Language.BLang.Homework.Homework4 (semanticCheck) where

import Language.BLang.Error (CompileError)
import Language.BLang.FrontEnd.ParsedAST (ParseTree, AST)
import Language.BLang.FrontEnd.LexToken

type EWriter = Writer [CompileError]

-- snd == [] -> scope name
-- snd == [a] -> scalar
-- snd == returnType : (args || [TVoid])
type Scope = (String, [Type])

baseScope = ("Entry Point", [])

getType :: [Scope] -> String -> Maybe [Type]
getType scope identitier = find ((== identifier) . fst) scope >>= (map check . snd)
  where
    check TPtr itype = getType itype >>= TPtr
    check TArray aststmts itype = getType itype >>= (TPtr aststmts)
    check TCustom innerid = getType innerid
    check nativeTypes = Just nativeTypes


concatScope :: EWriter [a] -> b -> ([a] -> b -> EWriter [a]) -> EWriter [a]
concatScope writer' input f =
  writer' >>= (\x -> fmap (++ x) (f x input))


semanticCheck :: (AST.ParseTree, AST.AST) -> [CompileError]
semanticCheck (NonTerminal parseTrees, asttops) =
  fst . runWriter $ foldl (concatScope checkTop) (Writer (baseScope, [])) (zip parseTrees asttops)


checkTop :: EWriter [Scope] -> (AST.ParseTree, AST.ASTTop) -> EWriter [Scope]
checkTop scope (NonTerminal parseTrees, VarDeclList astdecls) =
  foldl (concatScope checkDecl) scope (zip parseTrees, astdecls)

checkTop scope (NonTerminal parseTree, funcdecl) = do
  let funcScope = (funcName funcdecl, returnType funcdecl : funcArgs funcdecl)
  let newScope = scope >>= (funcScope : )

  return newScope


checkDecl scope = undefined

checkFuncDecl scope = undefined
