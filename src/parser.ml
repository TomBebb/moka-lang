open Ast
open Token
open Type

type error_kind =
  | Unexpected of token * string
  | Expected of token list * token * string

let error_msg = function
  | Unexpected (got, expected) ->
      "Unexpected " ^ s_token_def got ^ " while parsing " ^ expected
  | Expected (expected, got, name) ->
      "Expected one of "
      ^ String.concat "," (List.map s_token_def expected)
      ^ " but got " ^ s_token_def got ^ "while parsing " ^ name

exception Error of error_kind span

let next_is tks def = Stream.peek tks = Some def

let mk_pos ex first last =
  (ex, {pfile= first.pfile; pmin= first.pmin; pmax= last.pmax})

let mk ex (_, first) (_, last) = mk_pos ex first last

let mk_one def pos = (def, pos)

let expect_ident tks =
  let tk = Stream.next tks in
  match tk with
  | TIdent id, pos -> mk_one id pos
  | def, _ -> raise (Error (mk (Unexpected (def, "identifier")) tk tk))

let expect tks expected name =
  let tk = Stream.next tks in
  let def, pos = tk in
  if not (List.mem def expected) then
    raise (Error (mk (Expected (expected, def, name)) tk tk))
  else pos

let rec parse_expr tks =
  let parse_base_expr tks =
    let first, first_pos = Stream.next tks in
    match first with
    | TIdent id -> mk_one (EIdent id) first_pos
    | TConst c -> mk_one (EConst c) first_pos
    | TOpenParen ->
        let inner = parse_expr tks in
        let last = expect tks [TCloseParen] "parenthesis" in
        mk_pos (EParen inner) first_pos last
    | _ -> raise (Error (mk_one (Unexpected (first, "expression")) first_pos))
  in
  let rec parse_after_expr base tks =
    match Stream.peek tks with
    | Some (TDot, _) ->
        let _ = Stream.next tks in
        let field = expect_ident tks in
        let name, last = field in
        let _, first = base in
        parse_after_expr (mk_pos (EField (base, name)) first last) tks
    | _ -> base
  in
  let base = parse_base_expr tks in
  parse_after_expr base tks

let next_is tks want =
  match Stream.peek tks with Some (def, _) -> def = want | _ -> false

let parse_path tks =
  let parts = ref [expect_ident tks] in
  while next_is tks TDot do
    let _ = Stream.next tks in
    parts := !parts @ [expect_ident tks]
  done ;
  !parts

let parse_ty tks =
  let tk = Stream.next tks in
  let def, pos = tk in
  match def with
  | TKPrim p -> TPrim p
  | _ -> raise (Error (mk_one (Unexpected (def, "type")) pos))

let parse_member tks =
  let tk = Stream.next tks in
  let def, pos = tk in
  match def with
  | TKeyword KFunc ->
      let name, _ = expect_ident tks in
      let _ = expect tks [TOpenParen] "function declaration" in
      let _ = expect tks [TCloseParen] "function declaration" in
      let ret =
        if next_is tks TColon then
          let _ = Stream.next tks in
          parse_ty tks
        else TPrim TVoid
      in
      let ex = parse_expr tks in
      {mname= name; mkind= MFunc ([], TPrim TVoid, ex)}
  | _ -> raise (Error (mk_one (Unexpected (def, "member")) pos))

let parse_type_def tks =
  let tk = Stream.next tks in
  let def, pos = tk in
  match def with
  | TKeyword KClass ->
      let name = expect_ident tks in
      let ext =
        if next_is tks (TKeyword KExtends) then Some (parse_path tks) else None
      in
      ()
  | _ -> raise (Error (mk_one (Unexpected (def, "type definition")) pos))
