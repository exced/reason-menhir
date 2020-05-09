[@deriving show]
type pos =
  Lexing.position = {
    pos_fname: string,
    pos_lnum: int,
    pos_bol: int,
    pos_cnum: int,
  };

[@deriving show]
type literal = [
  | `Null
  | `Bool(bool)
  | `String(string)
  | `Number([ | `Dec(string) | `Hex(string)])
];

[@deriving show]
type unary_op = [
  | `PostIncr
  | `PostDecr
  | `PreIncr
  | `PreDecr
  | `Not
  | `Negate
  | `Plus
  | `Minus
  | `TypeOf
  | `Void
  | `Delete
];

[@deriving show]
type binary_op = [
  | `Eq
  | `Neq
  | `Equal
  | `NotEqual
  | `Lt
  | `LtE
  | `Gt
  | `GtE
  | `LShift
  | `AShift
  | `RShift
  | `Add
  | `Sub
  | `Mult
  | `Div
  | `Mod
  | `BitOr
  | `BitAnd
  | `BitXor
  | `In
  | `InstanceOf
  | `Or
  | `And
];

[@deriving show]
type assign_op = [
  | `Nop
  | `Add
  | `Sub
  | `Mult
  | `Div
  | `Mod
  | `LShift
  | `AShift
  | `RShift
  | `BitOr
  | `BitXor
  | `BitAnd
];

[@deriving show]
type program = list(stmt)
[@deriving show]
and stmt =
  | Empty(pos)
  | Block(list(stmt), pos)
  | Expr(expr, pos)
  | If(expr, stmt, option(stmt), pos)
  | Labeled(string, stmt, pos)
  | Break(option(string), pos)
  | Continue(option(string), pos)
  | With(expr, stmt, pos)
  | Switch(expr, list((option(expr), list(stmt))), pos)
  | Return(option(expr), pos)
  | Throw(expr, pos)
  | Try(
      list(stmt),
      option((string, list(stmt))),
      option(list(stmt)),
      pos,
    )
  | While(expr, stmt, pos)
  | Do(stmt, expr, pos)
  | For(
      [ | `Nop | `Var(list((string, option(expr), pos))) | `Expr(expr)],
      option(expr),
      option(expr),
      stmt,
      pos,
    )
  | ForIn(
      [ | `Var(string, option(expr), pos) | `Expr(expr)],
      expr,
      stmt,
      pos,
    )
  | Debugger(pos)
  | FunctionDecl(string, list(string), list(stmt), pos)
  | Var(list((string, option(expr), pos)), pos)
[@deriving show]
and expr =
  | Ident(string, pos)
  | Literal(literal, pos)
  | This(pos)
  | Array(list(expr), pos)
  | Object(list(property), pos)
  | Function(option(string), list(string), list(stmt), pos)
  | Sequence(expr, expr, pos)
  | Unary(unary_op, expr, pos)
  | Binary(binary_op, expr, expr, pos)
  | Assign(assign_op, expr, expr, pos)
  | Ternary(expr, expr, expr, pos)
  | New(expr, list(expr), pos)
  | Call(expr, list(expr), pos)
  | Member(expr, [ | `Ident(string) | `Expr(expr)], pos)
  | RegExp(expr, expr, pos)
[@deriving show]
and property_name = [ | `Ident(string) | literal]
[@deriving show]
and property = [
  | `Init(property_name, expr, pos)
  | `Get(property_name, list(stmt), pos)
  | `Set(property_name, string, list(stmt), pos)
];

let pos_of_expr =
  fun
  | Ident(_, pos) => pos
  | Literal(_, pos) => pos
  | This(pos) => pos
  | Array(_, pos) => pos
  | Object(_, pos) => pos
  | Function(_, _, _, pos) => pos
  | Sequence(_, _, pos) => pos
  | Unary(_, _, pos) => pos
  | Binary(_, _, _, pos) => pos
  | Assign(_, _, _, pos) => pos
  | Ternary(_, _, _, pos) => pos
  | New(_, _, pos) => pos
  | Call(_, _, pos) => pos
  | Member(_, _, pos) => pos
  | RegExp(_, _, pos) => pos;

let pos_of_stmt =
  fun
  | Empty(pos) => pos
  | Block(_, pos) => pos
  | Expr(_, pos) => pos
  | If(_, _, _, pos) => pos
  | Labeled(_, _, pos) => pos
  | Break(_, pos) => pos
  | Continue(_, pos) => pos
  | With(_, _, pos) => pos
  | Switch(_, _, pos) => pos
  | Return(_, pos) => pos
  | Throw(_, pos) => pos
  | Try(_, _, _, pos) => pos
  | While(_, _, pos) => pos
  | Do(_, _, pos) => pos
  | For(_, _, _, _, pos) => pos
  | ForIn(_, _, _, pos) => pos
  | Debugger(pos) => pos
  | FunctionDecl(_, _, _, pos) => pos
  | Var(_, pos) => pos;

let print = program => Format.printf("%s", show_program(program));