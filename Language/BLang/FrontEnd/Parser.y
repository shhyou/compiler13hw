{
module Language.BLang.FrontEnd.Parser (module AST, parse) where
import qualified Language.BLang.FrontEnd.ParsedAST as AST
import Language.BLang.FrontEnd.Lexer  as Lex (Token(..), Literal(..), lexer)
import Language.BLang.FrontEnd.ParseMonad (Parser, runParser)
}

%name parser
%tokentype { Lex.Token }
%error { parseError }
%monad { Parser }
%lexer { Lex.lexer }{ Lex.EOF }

-- Still, the order is essential.
-- It corresponds to the order where parameters are being pattern matched
%token
  LITERAL    { Lex.LiteralToken $$ }
  KW_INT     { Lex.ID "int" }
  KW_FLOAT   { Lex.ID "float" }
  KW_VOID    { Lex.ID "void" }
  KW_IF      { Lex.ID "if" }
  KW_ELSE    { Lex.ID "else" }
  KW_WHILE   { Lex.ID "while" }
  KW_FOR     { Lex.ID "for" }
  KW_TYPEDEF { Lex.ID "typedef" }
  KW_RETURN  { Lex.ID "return" }

  IDENTIFIER { Lex.ID $$ }

  OP_ASSGN   { Lex.SymAssign }

  OP_AND       { Lex.SymLogic "&&" }
  OP_OR        { Lex.SymLogic "||" }
  OP_NOT       { Lex.SymLogic "!" }

  OP_LT        { Lex.SymRelational "<" }
  OP_GT        { Lex.SymRelational ">" }
  OP_LEQ       { Lex.SymRelational "<=" }
  OP_GEQ       { Lex.SymRelational ">=" }
  OP_EQ        { Lex.SymRelational "==" }
  OP_NEQ       { Lex.SymRelational "!=" }

  OP_PLUS      { Lex.SymArithmetic "+" }
  OP_MINUS     { Lex.SymArithmetic "-" }
  OP_TIMES     { Lex.SymArithmetic "*" }
  OP_DIVIDE    { Lex.SymArithmetic "/" }

  LIT_INT      { Lex.LiteralToken (IntLiteral $$) }
  LIT_FLOAT    { Lex.LiteralToken (FloatLiteral $$) }
  LIT_STRING   { Lex.LiteralToken (StringLiteral $$) }

  MK_LPAREN    { Lex.SymSeparator "(" }
  MK_RPAREN    { Lex.SymSeparator ")" }
  MK_LBRACE    { Lex.SymSeparator "{" }
  MK_RBRACE    { Lex.SymSeparator "}" }
  MK_LSQBRACE  { Lex.SymSeparator "[" }
  MK_RSQBRACE  { Lex.SymSeparator "]" }
  MK_COMMA     { Lex.SymSeparator "," }
  MK_SEMICOLON { Lex.SymSeparator ";" }
  MK_DOT       { Lex.SymSeparator "." }

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

parseError :: Lex.Token -> Parser a
parseError token = fail $ "Parse error: got token '" ++ show token ++ "'"
}
