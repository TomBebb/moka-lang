(** Parses a stream of tokens as a source file AST *)

open Ast
open Token
open Type
open Core_kernel

type error_kind =
  | Unexpected of token * string
  | Expected of token list * token * string

let error_msg = function
  | Unexpected (got, expected) ->
      sprintf "Unexpected '%s' while parsing %s" (s_token_def got) expected
  | Expected ([expected], got, name) ->
      sprintf "Expected '%s' but got '%s' while parsing %s"
        (s_token_def expected) (s_token_def got) name
  | Expected (expected, got, name) ->
      sprintf "Expected one of %s but got '%s' while parsing %s"
        (String.concat ~sep:", "
           (List.map ~f:(fun tk -> "'" ^ s_token_def tk ^ "'") expected))
        (s_token_def got) name

exception Error of error_kind span

(** Check if next token in stream [tks] if [want] *)
let next_is tks want =
  match Stream.peek tks with Some (def, _) -> def = want | _ -> false

let mk_pos ex first last =
  (ex, {pfile= first.pfile; pmin= first.pmin; pmax= last.pmax})

let mk ex (_, first) (_, last) = mk_pos ex first last

let mk_one def pos = (def, pos)

(** Expect an identifier in token stream [tks], or raise an error*)
let expect_ident tks =
  let tk = Stream.next tks in
  match tk with
  | TIdent id, pos -> mk_one id pos
  | def, _ -> raise (Error (mk (Unexpected (def, "identifier")) tk tk))

(** Expect a token [expected] in token stream [tks], or raise an error *)
let expect tks expected name =
  let tk = Stream.next tks in
  let def, pos = tk in
  if not (List.mem expected def ~equal:(fun a b -> a = b)) then
    raise (Error (mk (Expected (expected, def, name)) tk tk))
  else pos

let rec last = function
  | [item] -> item
  | [] -> assert false
  | _ :: rest -> last rest

(** Parse a package from token stream [tks] *)
let parse_pack tks =
  let first, _ = expect_ident tks in
  let parts = ref [first] in
  while next_is tks TDot do
    ignore (Stream.next tks) ;
    let part, _ = expect_ident tks in
    parts := !parts @ [part]
  done ;
  !parts

(** Parse the path from token stream [tks] *)
let parse_path tks =
  let parts = parse_pack tks in
  let name = last parts in
  let parts =
    Array.sub (Array.of_list parts) ~pos:0 ~len:(List.length parts - 1)
  in
  (Array.to_list parts, name)

let local_pack = ref []

(** Parse a type from token stream [tks] *)
let rec parse_ty tks =
  let rec parse_types delim last =
    match Stream.peek tks with
    | Some (def, _) when def = last -> []
    | _ -> (
        let ty = parse_ty tks in
        match Stream.peek tks with
        | Some (def, _) when def = delim ->
            ignore (Stream.next tks) ;
            [ty] @ parse_types delim last
        | _ -> [ty] )
  in
  match Stream.peek tks with
  | Some (TIdent _, _) -> TPath (parse_path tks)
  | Some (TKPrim p, _) ->
      ignore (Stream.next tks) ;
      TPrim p
  | Some (TOpenParen, _) ->
      ignore (Stream.next tks) ;
      let mems = parse_types TComma TCloseParen in
      ignore (expect tks [TCloseParen] "Tuple declaration") ;
      TTuple mems
  | Some (def, pos) -> raise (Error (mk_one (Unexpected (def, "type")) pos))
  | None -> raise (Failure "unexpected eof parsing type")

(** Parse an expression from token stream [tks] in [kind]*)
let rec parse_expr kind tks =
  let rec parse_exprs kind tks term sep =
    if next_is tks term then []
    else
      let ex : expr = parse_expr kind tks in
      let peek : token span option = Stream.peek tks in
      match (sep, peek) with
      | None, Some (_, _) -> [ex] @ parse_exprs kind tks term None
      | Some sep, Some (tk, _) when tk = sep ->
          ignore (Stream.next tks) ;
          [ex] @ parse_exprs kind tks term (Some sep)
      | _ -> [ex]
  in
  let parse_base_expr tks =
    let first, first_pos = Stream.next tks in
    match first with
    | TIdent id -> mk_one (EIdent id) first_pos
    | TConst c -> mk_one (EConst c) first_pos
    | TUnOp op ->
        let inner = parse_expr "unary operand" tks in
        let _, last = inner in
        mk_pos (EUnOp (op, inner)) first_pos last
    | TKeyword KThis -> mk_one EThis first_pos
    | TKeyword KSuper -> mk_one ESuper first_pos
    | TKeyword KReturn ->
        let inner =
          if next_is tks TSemicolon then (
            ignore (Stream.next tks) ;
            None )
          else Some (parse_expr "return value" tks)
        in
        mk_one (EReturn inner) first_pos
    | TKeyword KNew ->
        let path = parse_path tks in
        ignore (expect tks [TOpenParen] "constructor call") ;
        let args =
          parse_exprs "constructor arguments" tks TCloseParen (Some TComma)
        in
        let last = expect tks [TCloseParen] "constructor call" in
        mk_pos (ENew (path, args)) first_pos last
    | TKeyword KWhile ->
        let cond = parse_expr "while condition" tks in
        let body = parse_expr "while body" tks in
        let _, last = body in
        mk_pos (EWhile (cond, body)) first_pos last
    | TKeyword KBreak -> mk_one EBreak first_pos
    | TKeyword KContinue -> mk_one EContinue first_pos
    | TKeyword ((KVar | KVal) as kind) ->
        let name, _ = expect_ident tks in
        let ty =
          if next_is tks TColon then
            Some
              (let _ = Stream.next tks in
               parse_ty tks)
          else None
        in
        let _ = expect tks [TBinOp OpAssign] "variable declaration" in
        let value = parse_expr "variable value" tks in
        let _, last_pos = value in
        mk_pos
          (EVar ((if kind = KVar then Variable else Constant), ty, name, value))
          first_pos last_pos
    | TOpenParen ->
        let inner = parse_expr "tuple or parenthesis" tks in
        if next_is tks TComma then (
          ignore (Stream.next tks) ;
          let mems =
            [inner]
            @ parse_exprs "tuple declaration" tks TCloseParen (Some TComma)
          in
          let last_pos = expect tks [TCloseParen] "tuple declaration" in
          mk_pos (ETuple mems) first_pos last_pos )
        else
          let last = expect tks [TCloseParen] "parenthesis" in
          mk_pos (EParen inner) first_pos last
    | TOpenBrace ->
        let exs = parse_exprs "block" tks TCloseBrace None in
        let last = expect tks [TCloseBrace] "block" in
        mk_pos (EBlock exs) first_pos last
    | TKeyword KIf ->
        let cond = parse_expr "if condition" tks in
        let body = parse_expr "if body" tks in
        let else_bod =
          if next_is tks (TKeyword KElse) then
            Some
              (let _ = Stream.next tks in
               parse_expr "else body" tks)
          else None
        in
        mk_pos (EIf (cond, body, else_bod)) first_pos first_pos
    | _ -> raise (Error (mk_one (Unexpected (first, kind)) first_pos))
  in
  let rec parse_after_expr base tks =
    let _, first = base in
    match Stream.peek tks with
    | Some (TDot, _) ->
        let _ = Stream.next tks in
        let field = expect_ident tks in
        let name, last = field in
        parse_after_expr (mk_pos (EField (base, name)) first last) tks
    | Some (TOpenParen, _) ->
        let _ = Stream.next tks in
        let args =
          parse_exprs "function arguments" tks TCloseParen (Some TComma)
        in
        let last = expect tks [TCloseParen] "function call" in
        parse_after_expr (mk_pos (ECall (base, args)) first last) tks
    | Some (TOpenBracket, _) ->
        ignore (Stream.next tks) ;
        let ind = parse_expr "index" tks in
        let last = expect tks [TCloseBracket] "index" in
        parse_after_expr (mk_pos (EArrayIndex (base, ind)) first last) tks
    | Some (TBinOp op, _) ->
        let _ = Stream.next tks in
        let other = parse_expr "binary operand" tks in
        parse_after_expr (mk (EBinOp (op, base, other)) base other) tks
    | Some (TKeyword KAs, _) ->
        ignore (Stream.next tks) ;
        let target = parse_ty tks in
        parse_after_expr (mk (ECast (base, target)) base base) tks
    | _ -> base
  in
  let base = parse_base_expr tks in
  parse_after_expr base tks

(** Expect a constant in token stream [tks] *)
let expect_const tks =
  let def, pos = Stream.next tks in
  match def with
  | TConst c -> c
  | _ -> raise (Error (mk_one (Unexpected (def, "constant")) pos))

(** Parse attributes in token stream [tks] *)
let parse_atts tks =
  let atts = Hashtbl.Poly.create () in
  while next_is tks TAt do
    ignore (Stream.next tks) ;
    let name, _ = expect_ident tks in
    ignore (expect tks [TOpenParen] "attribute") ;
    let v = expect_const tks in
    ignore (expect tks [TCloseParen] "attribute") ;
    Printf.printf "Parsed attr: %s = %s\n" name (s_const v) ;
    ignore (Hashtbl.add atts ~key:name ~data:v)
  done ;
  atts

(* Attemt to parse a member modifier in token stream [tks] *)
let parse_member_mod tks =
  match Stream.peek tks with
  | Some (TKeyword KVirtual, _) ->
      let _ = Stream.next tks in
      Some MVirtual
  | Some (TKeyword KOverride, _) ->
      let _ = Stream.next tks in
      Some MOverride
  | Some (TKeyword KExtern, _) ->
      let _ = Stream.next tks in
      Some MExtern
  | Some (TKeyword KStatic, _) ->
      let _ = Stream.next tks in
      Some MStatic
  | Some (TKeyword KPublic, _) ->
      let _ = Stream.next tks in
      Some MPublic
  | Some (TKeyword KPrivate, _) ->
      let _ = Stream.next tks in
      Some MPrivate
  | _ -> None

(* Parse member modifiers in token stream [tks] *)
let rec parse_member_mods tks mods =
  match parse_member_mod tks with
  | Some md ->
      mods := Set.add !mods md ;
      parse_member_mods tks mods
  | _ -> ()

(* Parse method parameters *)
let rec parse_params tks term =
  if next_is tks term then []
  else
    let name, _ = expect_ident tks in
    let _ = expect tks [TColon] "parameters" in
    let ty = parse_ty tks in
    let param = {pname= name; ptype= ty} in
    if next_is tks TComma then
      let _ = expect tks [TComma] "parameters" in
      param :: parse_params tks term
    else [param]

(* Parse struct/class member *)
let parse_member tks =
  let atts = parse_atts tks in
  let mods : member_mods ref = ref (Set.Poly.of_list []) in
  parse_member_mods tks mods ;
  let is_extern = Set.mem !mods MExtern && Set.mem !mods MStatic in
  let tk = Stream.next tks in
  let def, pos = tk in
  match def with
  | TKeyword KFunc ->
      let (name, _), is_new =
        if next_is tks (TKeyword KNew) then (
          ignore (Stream.next tks) ;
          (("new", pos), true) )
        else (expect_ident tks, false)
      in
      let _ = expect tks [TOpenParen] "function declaration" in
      let args = parse_params tks TCloseParen in
      let _ = expect tks [TCloseParen] "function declaration" in
      let ret =
        if next_is tks TColon then
          let _ = Stream.next tks in
          parse_ty tks
        else TPrim TVoid
      in
      let ex =
        if is_extern then (EBlock [], pos) else parse_expr "function body" tks
      in
      ( { mname= name
        ; mkind= (if is_new then MConstr (args, ex) else MFunc (args, ret, ex))
        ; mmods= !mods
        ; matts= atts }
      , pos )
  | TKeyword ((KVar | KVal) as v) ->
      let name, _ = expect_ident tks in
      let ty =
        if next_is tks TColon then
          let _ = Stream.next tks in
          Some (parse_ty tks)
        else None
      in
      let ex =
        if next_is tks (TBinOp OpAssign) then
          let _ = Stream.next tks in
          let tk, pos = Stream.next tks in
          let c =
            match tk with
            | TConst cn -> cn
            | def ->
                raise
                  (Error (mk_one (Unexpected (def, "member constant")) pos))
          in
          Some c
        else None
      in
      ( { mname= name
        ; mkind= MVar ((if v = KVar then Variable else Constant), ty, ex)
        ; mmods= !mods
        ; matts= atts }
      , pos )
  | _ -> raise (Error (mk_one (Unexpected (def, "member")) pos))

(** Parse type members in token stream [tks] with terminator [term]*)
let rec parse_members tks term =
  if next_is tks term then []
  else
    let mem = parse_member tks in
    mem :: parse_members tks term

let parse_type_def tks =
  let tk = Stream.next tks in
  let def, start = tk in
  match def with
  | TKeyword KStruct ->
      let name, _ = expect_ident tks in
      let _ = expect tks [TOpenBrace] "struct declaration" in
      let members = parse_members tks TCloseBrace in
      let last = expect tks [TCloseBrace] "struct declaration" in
      ( { epath= (!local_pack, name)
        ; emembers= members
        ; ekind= EStruct
        ; emods= Set.Poly.empty }
      , {pfile= start.pfile; pmin= start.pmin; pmax= last.pmax} )
  | TKeyword KClass ->
      let name, _ = expect_ident tks in
      let ext =
        if next_is tks (TKeyword KExtends) then (
          ignore (Stream.next tks) ;
          Some (parse_path tks) )
        else None
      in
      let cl = {cextends= ext; cimplements= []} in
      let _ = expect tks [TOpenBrace] "class declaration" in
      let members = parse_members tks TCloseBrace in
      let last = expect tks [TCloseBrace] "class declaration" in
      ( { epath= (!local_pack, name)
        ; emembers= members
        ; ekind= EClass cl
        ; emods= Set.Poly.empty }
      , {pfile= start.pfile; pmin= start.pmin; pmax= last.pmax} )
  | _ -> raise (Error (mk_one (Unexpected (def, "type definition")) start))

(** Parse the module in token stream [tks] *)
let parse_mod tks =
  let rec parse_imports () =
    match Stream.peek tks with
    | Some (TKeyword KImport, _) ->
        ignore (Stream.next tks) ;
        let path = parse_path tks in
        [path] @ parse_imports ()
    | _ -> []
  in
  let rec parse_type_defs () =
    if match Stream.peek tks with None | Some (TEof, _) -> true | _ -> false
    then []
    else
      let def = parse_type_def tks in
      [def] @ parse_type_defs ()
  in
  (local_pack :=
     match Stream.peek tks with
     | Some (TKeyword KPackage, _) ->
         ignore (Stream.next tks) ;
         parse_pack tks
     | _ -> []) ;
  let imports = parse_imports () in
  let defs = parse_type_defs () in
  {mpackage= !local_pack; mimports= imports; mdefs= defs}
