{
module FrontEnd.Parser (parse) where
import FrontEnd.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  'hello' { Hello }

%%

initial :: { Int }
        : 'hello' { 42 }

{
parseError = undefined
}
