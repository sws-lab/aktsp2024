/* Regulaaravaldise parser ocamlyacc-iga.
   Vt. https://v2.ocaml.org/manual/lexyacc.html. */

%token EMPTY
%token EPS
%token<char> CHAR
%token CHOICE
%token STAR
%token LPAREN RPAREN
%token EOF

%type<Re.t> full_re
%start full_re
%%

full_re:
  | re EOF { $1 }
;

re:
  | choice_re { $1 }
;

choice_re:
  | concat_re { $1 }
  | choice_re CHOICE concat_re { Choice ($1, $3) }
;

concat_re:
  | { Eps }
  | ne_concat_re { $1 }
;

ne_concat_re:
  | star_re { $1 }
  | ne_concat_re star_re { Concat ($1, $2) }
;

star_re:
  | atom_re { $1 }
  | atom_re STAR { Star $1 }
;

atom_re:
  | EMPTY { Empty }
  | EPS { Eps }
  | CHAR { Char $1 }
  | LPAREN re RPAREN { $2 }
;
