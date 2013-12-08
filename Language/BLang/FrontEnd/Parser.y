{
module Language.BLang.FrontEnd.ParserHappy (parse) where
import Control.Applicative ((<$>))
import Control.Monad.Error
import Control.Monad.State
import Data.Monoid
import Data.Foldable (foldMap)

import Language.BLang.Data
import Language.BLang.Error
import qualified Language.BLang.FrontEnd.ParsedAST as AST
import qualified Language.BLang.FrontEnd.Lexer as Lexer (Token(..), Literal(..), showToken, getTokenData, lexer)
import Language.BLang.FrontEnd.ParseMonad (Parser, runParser, getCurrLine)
}

%name parser
%tokentype { Lexer.Token Line }
%error { parseError }
%monad { Parser }
%lexer { Lexer.lexer }{ Lexer.EOF _ }

-- Still, the order is essential.
-- It corresponds to the order where parameters are being pattern matched
%token
  LITERAL    { Lexer.LiteralToken $$ _ }
  KW_INT     { Lexer.Identifier "int" _ }
  KW_FLOAT   { Lexer.Identifier "float" _ }
  KW_VOID    { Lexer.Identifier "void" _ }
  KW_IF      { Lexer.Identifier "if" _ }
  KW_ELSE    { Lexer.Identifier "else" _ }
  KW_WHILE   { Lexer.Identifier "while" _ }
  KW_FOR     { Lexer.Identifier "for" _ }
  KW_TYPEDEF { Lexer.Identifier "typedef" _ }
  KW_RETURN  { Lexer.Identifier "return" _ }

  IDENTIFIER { Lexer.Identifier $$ _ }

  OP_ASSIGN   { Lexer.SymAssign _ }

  OP_AND       { Lexer.SymLogic "&&" _ }
  OP_OR        { Lexer.SymLogic "||" _ }
  OP_NOT       { Lexer.SymLogic "!" _ }

  OP_LT        { Lexer.SymRelational "<" _ }
  OP_GT        { Lexer.SymRelational ">" _ }
  OP_LEQ       { Lexer.SymRelational "<=" _ }
  OP_GEQ       { Lexer.SymRelational ">=" _ }
  OP_EQ        { Lexer.SymRelational "==" _ }
  OP_NEQ       { Lexer.SymRelational "!=" _ }

  OP_PLUS      { Lexer.SymArithmetic "+" _ }
  OP_MINUS     { Lexer.SymArithmetic "-" _ }
  OP_TIMES     { Lexer.SymArithmetic "*" _ }
  OP_DIVIDE    { Lexer.SymArithmetic "/" _ }

  LIT_INT      { Lexer.LiteralToken (Lexer.IntLiteral $$) _ }
  LIT_FLOAT    { Lexer.LiteralToken (Lexer.FloatLiteral $$) _ }
  LIT_STRING   { Lexer.LiteralToken (Lexer.StringLiteral $$) _ }

  MK_LPAREN    { Lexer.SymSeparator "(" _ }
  MK_RPAREN    { Lexer.SymSeparator ")" _ }
  MK_LBRACE    { Lexer.SymSeparator "{" _ }
  MK_RBRACE    { Lexer.SymSeparator "}" _ }
  MK_LSQBRACE  { Lexer.SymSeparator "[" _ }
  MK_RSQBRACE  { Lexer.SymSeparator "]" _ }
  MK_COMMA     { Lexer.SymSeparator "," _ }
  MK_SEMICOLON { Lexer.SymSeparator ";" _ }
  MK_DOT       { Lexer.SymSeparator "." _ }

%left OP_OR
%left OP_AND
%left OP_PLUS OP_MINUS
%left OP_TIMES OP_DIVIDE
%right OP_NOT OP_NEG

%%

program :: { AST.AST }
  : global_decl_list                           {% (return . reverse . $1) [] }
  | {- empty -}                                {% return [] }

global_decl_list :: { [AST.ASTTop] -> [AST.ASTTop] }
  : global_decl_list global_decl               {% return ($2 . $1) } -- reverse the order to be com-
  | global_decl                                {% return $1 }        -- patible with other `_list` rules

global_decl :: { [AST.ASTTop] -> [AST.ASTTop] }
  : decl_list function_decl                    {% return (($2:) . ((AST.VarDeclList (reverse $1)):)) }
  | function_decl                              {% return ($1:) }

function_decl :: { AST.ASTTop }
  : type IDENTIFIER
    MK_LPAREN param_list0 MK_RPAREN
    MK_LBRACE block MK_RBRACE                  {% return $ AST.FuncDecl
                                                  { AST.returnType = $1
                                                  , AST.funcName = $2
                                                  , AST.funcArgs = reverse $4
                                                  , AST.funcCode = $7 }}
  | IDENTIFIER IDENTIFIER
    MK_LPAREN param_list0 MK_RPAREN
    MK_LBRACE block MK_RBRACE                  {% return $ AST.FuncDecl
                                                  { AST.returnType = AST.TCustom $1
                                                  , AST.funcName = $2
                                                  , AST.funcArgs = reverse $4
                                                  , AST.funcCode = $7 }}
  | KW_VOID IDENTIFIER
    MK_LPAREN param_list0 MK_RPAREN
    MK_LBRACE block MK_RBRACE                  {% return $ AST.FuncDecl
                                                  { AST.returnType = AST.TVoid
                                                  , AST.funcName = $2
                                                  , AST.funcArgs = reverse $4
                                                  , AST.funcCode = $7 }}

param_list0 :: { [(String, AST.Type)] }
  : param_list                                 {% return $1 }
  | {- empty -}                                {% return [] }

param_list :: { [(String, AST.Type)] }
  : param_list MK_COMMA param                  {% return ($3:$1) }
  | param                                      {% return [$1] }

param :: { (String, AST.Type) }
  : param_var_type IDENTIFIER                  {% return ($2, $1) }
  | param_var_type IDENTIFIER dim_fn           {% return ($2, $3 $1) }

param_var_type :: { AST.Type }
  : KW_INT                                     {% return AST.TInt }
  | KW_FLOAT                                   {% return AST.TFloat }
  | IDENTIFIER                                 {% return (AST.TCustom $1) }

dim_fn :: { AST.Type -> AST.Type }
  : MK_LSQBRACE MK_RSQBRACE                    {% return AST.TPtr }
  | MK_LSQBRACE MK_RSQBRACE dim_fn_list        {% return (AST.TPtr . AST.TArray (reverse $3)) }
  | dim_fn_list                                {% return (AST.TArray (reverse $1)) }

dim_fn_list :: { [AST.ASTStmt] }
  : MK_LSQBRACE expr MK_RSQBRACE               {% return [$2] }
  | dim_fn_list MK_LSQBRACE expr MK_RSQBRACE   {% return ($3:$1) }

block :: { AST.ASTStmt }
  : decl_list stmt_list                        {% return (AST.Block (reverse $1) (reverse $2)) }
  | stmt_list                                  {% return (AST.Block [] (reverse $1)) }
  | decl_list                                  {% return (AST.Block (reverse $1) []) }
  | {- empty -}                                {% return (AST.Block [] [] ) }

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
  : type init_id_list MK_SEMICOLON             {% return $ AST.VarDecl $ map ($ $1) (reverse $2) }
  | IDENTIFIER init_id_list MK_SEMICOLON       {% return $ AST.VarDecl $ map ($ (AST.TCustom $1)) (reverse $2) }

type :: { AST.Type }
  : KW_INT                                     {% return AST.TInt }
  | KW_FLOAT                                   {% return AST.TFloat }

id_list :: { [AST.Type -> (String, AST.Type)] }
  : id_list MK_COMMA one_id                    {% return ($3:$1) }
  | one_id                                     {% return [$1] }

one_id :: { AST.Type -> (String, AST.Type) }
  : IDENTIFIER                                 {% return $ (,) $1 }
  | IDENTIFIER dim_decl                        {% return $ (,) $1 . AST.TArray (reverse $2) }

dim_decl :: { [AST.ASTStmt] }
  : dim_decl MK_LSQBRACE cexpr MK_RSQBRACE     {% return ($3:$1) }
  | MK_LSQBRACE cexpr MK_RSQBRACE              {% return [$2] }

cexpr :: { AST.ASTStmt }
  : cexpr OP_PLUS cexpr                        {% return (AST.Expr AST.Plus [$1, $3]) }
  | cexpr OP_MINUS cexpr                       {% return (AST.Expr AST.Minus [$1, $3]) }
  | cexpr OP_TIMES cexpr                       {% return (AST.Expr AST.Times [$1, $3]) }
  | cexpr OP_DIVIDE cexpr                      {% return (AST.Expr AST.Divide [$1, $3]) }
  | LITERAL                                    {% return (AST.LiteralVal $1) }
  | MK_LPAREN cexpr MK_RPAREN                  {% return $2 }

init_id_list :: { [AST.Type -> (String, AST.Type, Maybe AST.ASTStmt)] }
  : init_id_list MK_COMMA init_id              {% return ($3:$1) }
  | init_id                                    {% return [$1] }

init_id :: { AST.Type -> (String, AST.Type, Maybe AST.ASTStmt) }
  : IDENTIFIER                                 {% return (\t -> ($1, t, Nothing)) }
  | IDENTIFIER dim_decl                        {% return (\t -> ($1, AST.TArray (reverse $2) t, Nothing)) }
  | IDENTIFIER OP_ASSIGN relop_expr            {% return (\t -> ($1, t, Just $3)) }

stmt_list :: { [AST.ASTStmt] }
  : stmt_list stmt                             {% return ($2:$1) }
  | stmt                                       {% return [$1] }

stmt :: { AST.ASTStmt }
  : MK_LBRACE block MK_RBRACE                  {% return $2 }
  | KW_WHILE MK_LPAREN
      relop_expr_list
    MK_RPAREN stmt                             {% return $ AST.While
                                                  { AST.whileCond = reverse $3
                                                  , AST.whileCode = $5 }}
  | KW_FOR MK_LPAREN
      assign_expr_list0 MK_SEMICOLON
      relop_expr_list0 MK_SEMICOLON
      assign_expr_list0
    MK_RPAREN stmt                             {% return $ AST.For
                                                  { AST.forInit = reverse $3
                                                  , AST.forCond = reverse $5
                                                  , AST.forIter = reverse $7
                                                  , AST.forCode = $9 }}
  | IDENTIFIER MK_LPAREN
      relop_expr_list0
    MK_RPAREN MK_SEMICOLON                     {% return (AST.Ap (AST.Identifier $1) (reverse $3)) }
  | var_ref OP_ASSIGN relop_expr MK_SEMICOLON  {% return (AST.Expr AST.Assign [$1, $3]) }
  | KW_IF relop_expr stmt                      {% return (AST.If $2 $3 Nothing) }
  | KW_IF relop_expr stmt
    KW_ELSE stmt                               {% return (AST.If $2 $3 (Just $5)) }
  | KW_RETURN relop_expr MK_SEMICOLON          {% return (AST.Return (Just $2)) }
  | KW_RETURN MK_SEMICOLON                     {% return (AST.Return Nothing) }
  | MK_SEMICOLON                               {% return AST.Nop }

assign_expr_list0 :: { [AST.ASTStmt] }
  : assign_expr_list                           {% return $1 }
  | {- empty -}                                {% return [] }

assign_expr_list :: { [AST.ASTStmt] }
  : assign_expr_list MK_COMMA assign_expr      {% return ($3:$1) }
  | assign_expr                                {% return [$1] }

assign_expr :: { AST.ASTStmt }
  : IDENTIFIER OP_ASSIGN relop_expr            {% return (AST.Expr AST.Assign [AST.Identifier $1, $3]) }
  | relop_expr                                 {% return $1 }

relop_expr :: { AST.ASTStmt }
  : relop_expr OP_OR relop_expr                {% return (AST.Expr AST.LOr [$1, $3]) }
  | relop_expr OP_AND relop_expr               {% return (AST.Expr AST.LAnd [$1, $3]) }
  | expr                                       {% return $1 }
  | expr rel_op expr                           {% return (AST.Expr $2 [$1, $3]) }

rel_op :: { AST.Operator }
  : OP_EQ                                      {% return AST.EQ }
  | OP_GEQ                                     {% return AST.GEQ }
  | OP_LEQ                                     {% return AST.LEQ }
  | OP_NEQ                                     {% return AST.NEQ }
  | OP_GT                                      {% return AST.GT }
  | OP_LT                                      {% return AST.LT }

relop_expr_list0 :: { [AST.ASTStmt] }
  : relop_expr_list                            {% return $1 }
  | {- empty -}                                {% return [] }

relop_expr_list :: { [AST.ASTStmt] }
  : relop_expr_list MK_COMMA relop_expr        {% return ($3:$1) }
  | relop_expr                                 {% return [$1] }

expr :: { AST.ASTStmt }
  : expr OP_PLUS expr                          {% return (AST.Expr AST.Plus [$1, $3]) }
  | expr OP_MINUS expr                         {% return (AST.Expr AST.Minus [$1, $3]) }
  | expr OP_TIMES expr                         {% return (AST.Expr AST.Times [$1, $3]) }
  | expr OP_DIVIDE expr                        {% return (AST.Expr AST.Divide [$1, $3]) }
  | terminal_expr                              {% return $1 }
  | OP_NOT terminal_expr                       {% return (AST.Expr AST.LNot [$2]) }
  | OP_MINUS terminal_expr %prec OP_NEG        {% return (AST.Expr AST.Negate [$2]) }

terminal_expr :: { AST.ASTStmt }
  : LITERAL                                    {% return (AST.LiteralVal $1) }
  | MK_LPAREN relop_expr MK_RPAREN             {% return $2 }
  | IDENTIFIER
    MK_LPAREN relop_expr_list0 MK_RPAREN       {% return (AST.Ap (AST.Identifier $1) (reverse $3)) }
  | var_ref                                    {% return $1 }

var_ref :: { AST.ASTStmt }
  : IDENTIFIER                                 {% return (AST.Identifier $1) }
  | IDENTIFIER dim_list                        {% return ($2 (AST.Identifier $1)) }

dim_list :: { AST.ASTStmt -> AST.ASTStmt }
  : dim_list MK_LSQBRACE expr MK_RSQBRACE      {% return ((\term -> AST.ArrayRef term $3) . $1)}
  | MK_LSQBRACE expr MK_RSQBRACE               {% return (\term -> AST.ArrayRef term $2) }

{
parse :: String -> Either CompileError AST.AST
parse = runParser parser

parseError :: Lexer.Token Line -> Parser a
parseError token =
  throwError . errorAt (Lexer.getTokenData token) $
    "Parse error: got token '" ++ Lexer.showToken token ++ "'"
}
