{
module FrontEnd.Parser (module ParsedAST, parse) where
import qualified FrontEnd.ParsedAST as ParsedAST (AST(..)) 
import FrontEnd.Lexer (Token(..), Literal(..))
}

%name parse
%tokentype { Token }
%error { parseError }

-- Still, the order is essential.
-- It corresponds to the order where parameters are being pattern matched
%token
  LITERAL    { LiteralToken $$ }
  KW_INT     { ID "int" }
  KW_FLOAT   { ID "float" }
  KW_VOID    { ID "void" }
  KW_IF      { ID "if" }
  KW_ELSE    { ID "else" }
  KW_WHILE   { ID "while" }
  KW_FOR     { ID "for" }
  KW_TYPEDEF { ID "typedef" }
  KW_RETURN  { ID "return" }

  IDENTIFIER { ID $$ }

  OP_ASSGN   { SymAssign }

  OP_AND       { SymLogic "&&" }
  OP_OR        { SymLogic "||" }
  OP_NOT       { SymLogic "!" }

  OP_LT        { SymRelational "<" }
  OP_GT        { SymRelational ">" }
  OP_LEQ       { SymRelational "<=" }
  OP_GEQ       { SymRelational ">=" }

  OP_PLUS      { SymArithmetic "+" }
  OP_MINUS     { SymArithmetic "-" }
  OP_TIMES     { SymArithmetic "*" }
  OP_DIVIDE    { SymArithmetic "/" }

  LIT_INT      { LiteralToken (IntLiteral $$) }
  LIT_FLOAT    { LiteralToken (FloatLiteral $$) }
  LIT_STRING   { LiteralToken (StringLiteral $$) }

  MK_LPAREN    { SymSeparator "(" }
  MK_RPAREN    { SymSeparator ")" }
  MK_LBRACE    { SymSeparator "{" }
  MK_RBRACE    { SymSeparator "}" }
  MK_LSQBRACE  { SymSeparator "[" }
  MK_RSQBRACE  { SymSeparator "]" }
  MK_COMMA     { SymSeparator "," }
  MK_SEMICOLON { SymSeparator ";" }
  MK_DOT       { SymSeparator "." }

%%

initial :: { IO () }
        : KW_RETURN  { putStrLn "KW_RETURN" }
        | IDENTIFIER { putStrLn $1 }
        | OP_AND     { putStrLn "OP_AND" }

{
parseError = undefined
}
