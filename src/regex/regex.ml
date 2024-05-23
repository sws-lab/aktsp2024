include Re

let parse s =
  let lexbuf = Lexing.from_string s in
  Re_parser.full_re Re_lexer.token lexbuf

module Brzozowski = Brzozowski
module Simplify = Simplify
