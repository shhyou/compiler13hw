{
module Language.BLang.FrontEnd.Parser (module AST, parse) where
import qualified Language.BLang.FrontEnd.ParsedAST as AST
import Language.BLang.FrontEnd.Lexer as Lexer (Token(..), Literal(..), lexer)
import Language.BLang.FrontEnd.ParseMonad (Parser, runParser)
}

%name parser
%tokentype { Lexer.Token }
%error { parseError }
%monad { Parser }
%lexer { Lexer.lexer }{ Lexer.EOF }

-- Still, the order is essential.
-- It corresponds to the order where parameters are being pattern matched
%token
  LITERAL    { Lexer.LiteralToken $$ }
  KW_INT     { Lexer.ID "int" }
  KW_FLOAT   { Lexer.ID "float" }
  KW_VOID    { Lexer.ID "void" }
  KW_IF      { Lexer.ID "if" }
  KW_ELSE    { Lexer.ID "else" }
  KW_WHILE   { Lexer.ID "while" }
  KW_FOR     { Lexer.ID "for" }
  KW_TYPEDEF { Lexer.ID "typedef" }
  KW_RETURN  { Lexer.ID "return" }

  IDENTIFIER { Lexer.ID $$ }

  OP_ASSGN   { Lexer.SymAssign }

  OP_AND       { Lexer.SymLogic "&&" }
  OP_OR        { Lexer.SymLogic "||" }
  OP_NOT       { Lexer.SymLogic "!" }

  OP_LT        { Lexer.SymRelational "<" }
  OP_GT        { Lexer.SymRelational ">" }
  OP_LEQ       { Lexer.SymRelational "<=" }
  OP_GEQ       { Lexer.SymRelational ">=" }
  OP_EQ        { Lexer.SymRelational "==" }
  OP_NEQ       { Lexer.SymRelational "!=" }

  OP_PLUS      { Lexer.SymArithmetic "+" }
  OP_MINUS     { Lexer.SymArithmetic "-" }
  OP_TIMES     { Lexer.SymArithmetic "*" }
  OP_DIVIDE    { Lexer.SymArithmetic "/" }

  LIT_INT      { Lexer.LiteralToken (IntLiteral $$) }
  LIT_FLOAT    { Lexer.LiteralToken (FloatLiteral $$) }
  LIT_STRING   { Lexer.LiteralToken (StringLiteral $$) }

  MK_LPAREN    { Lexer.SymSeparator "(" }
  MK_RPAREN    { Lexer.SymSeparator ")" }
  MK_LBRACE    { Lexer.SymSeparator "{" }
  MK_RBRACE    { Lexer.SymSeparator "}" }
  MK_LSQBRACE  { Lexer.SymSeparator "[" }
  MK_RSQBRACE  { Lexer.SymSeparator "]" }
  MK_COMMA     { Lexer.SymSeparator "," }
  MK_SEMICOLON { Lexer.SymSeparator ";" }
  MK_DOT       { Lexer.SymSeparator "." }

%%

program :: { AST.AST }
  : global_decl_list                           {% return $ reverse $1 }
  | {- empty -}                                {% return [] }

global_decl_list :: { [AST.ASTTop] }
  : global_decl_list decl_list                 {% return (AST.VarDeclList (reverse $2):$1) }
  | global_decl_list function_decl             {% return ($2:$1) }
  | decl_list                                  {% return [AST.VarDeclList $1] }
  | function_decl                              {% return [$1] }

function_decl :: { AST.ASTTop }
  : {- not implemented yet -}                  {% undefined }

decl_list :: { [AST.ASTDecl] }
  : decl_list decl                             {% return ($2:$1) }
  | decl                                       {% return [$1] }

decl :: { AST.ASTDecl }
  : type_decl                                  {% return $1 }
  | var_decl                                   {% return $1 }

type_decl :: { AST.ASTDecl }
  : KW_TYPEDEF type id_list MK_SEMICOLON       {% return $ AST.TypeDecl $ map ($ $2) (reverse $3) }
  | KW_TYPEDEF KW_VOID id_list MK_SEMICOLON    {% return $ AST.TypeDecl $ map ($ AST.TVoid) (reverse $3) }

var_decl :: { AST.ASTDecl }
  : {- not implemented yet -}                  {% undefined }

type :: { AST.Type }
  : KW_INT                                     {% return AST.TInt }
  | KW_FLOAT                                   {% return AST.TFloat }

id_list :: { [AST.Type -> (String, AST.Type)] }
  : id_list MK_COMMA one_id                    {% return ($3:$1) }
  | one_id                                     {% return [$1] }

one_id :: { AST.Type -> (String, AST.Type) }
  : IDENTIFIER                                 {% return $ (,) $1 }
  | IDENTIFIER dim_decl                        {% return $ (,) $1
                                                         . AST.TArray (reverse $2) }

dim_decl :: { [Integer] }
  : dim_decl MK_LSQBRACE cexpr MK_RSQBRACE     {% return ($3:$1) }
  | MK_LSQBRACE cexpr MK_RSQBRACE              {% return [$2] }

cexpr : {- not implemented -}                  {% undefined }

{
-- parse :: String -> Either ParseError a, where `a` is result type of `program`
parse = runParser parser

parseError :: Lexer.Token -> Parser a
parseError token = fail $ "Parse error: got token '" ++ show token ++ "'"
}
