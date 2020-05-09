open Parser;

let parse_from_lexbuf = lexbuf => Parser.program(Lexer.token(), lexbuf);

let parse_from_channel = c => parse_from_lexbuf(Lexing.from_channel(c));

let parse_from_file = file => {
  let c = open_in(file);
  try({
    let ast = parse_from_channel(c);
    close_in(c);
    ast;
  }) {
  | e =>
    close_in(c);
    raise(e);
  };
};

let print_ast = filename => {
  let program = parse_from_file(filename);
  Ast.print(program);
}
