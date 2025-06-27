%{
  open Ast
%}

%token <int> INTEGER
%token <string> IDENTIFIER
%token <string> STRING
%token DOLLAR
%token DOT
%token ASTERISK
%token DOUBLE_ASTERISK
%token LBRACKET
%token RBRACKET
%token MINUS
%token TO
%token LAST
%token EOF

%start <t> path_expression
%%

path_expression:
  | DOLLAR legs = list(path_leg) EOF { { scope = "$"; legs } }

path_leg:
  | member { $1 }
  | array_location { $1 }
  | DOUBLE_ASTERISK { DoubleWildcard }

member:
  | DOT key_name = key_name { Member key_name }
  | DOT ASTERISK { MemberWildcard }

key_name:
  | id = IDENTIFIER { id }
  | s = STRING { s }

array_location:
  | LBRACKET idx = array_index RBRACKET { ArrayAccess idx }
  | LBRACKET range = array_range RBRACKET { ArrayRange range }

array_index:
  | n = INTEGER { Index n }
  | ASTERISK { Wildcard }
  | LAST { Last }
  | LAST MINUS n = INTEGER { LastOffset n }

array_range:
  | start = array_index TO end_idx = array_index { { start_idx = start; end_idx } }

%%
