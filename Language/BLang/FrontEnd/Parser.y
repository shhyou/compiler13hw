{
module Language.BLang.FrontEnd.Parser (module ParsedAST, parse) where
import qualified Language.BLang.FrontEnd.ParsedAST as ParsedAST (AST(..)) 
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

program :: { () }
        : {- empty -}      {% undefined }

{
-- parse :: String -> Either ParseError a, where `a` is result type of `program`
parse = runParser parser

parseError :: Lex.Token -> Parser a
parseError token = fail $ "Parse error: got token '" ++ show token ++ "'"
}
