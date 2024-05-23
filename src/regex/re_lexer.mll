(* Regulaaravaldise lexer ocamllex-iga.
  Vt. https://v2.ocaml.org/manual/lexyacc.html. *)

{
open Re_parser
}

rule token = parse
  | "∅" { EMPTY }
  | "ε" { EPS }
  | '|' { CHOICE }
  | '*' { STAR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | _ as c { CHAR c }
  | eof { EOF }
