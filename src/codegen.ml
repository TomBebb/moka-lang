open Llvm
open Type
open Ast

type gen_ctx =
  { gen_ctx: llcontext
  ; gen_mod: llmodule
  ; gen_builder: llbuilder
  ; gen_structs: (path, lltype) Hashtbl.t }

let init () =
  let ctx = create_context () in
  let main_mod = create_module ctx "main" in
  let builder = builder ctx in
  { gen_ctx= ctx
  ; gen_mod= main_mod
  ; gen_builder= builder
  ; gen_structs= Hashtbl.create 10 }

let gen_ty ctx ty =
  match ty with
  | TPrim TVoid -> Llvm.void_type ctx.gen_ctx
  | TPrim TBool -> Llvm.i1_type ctx.gen_ctx
  | TPrim TByte -> Llvm.i8_type ctx.gen_ctx
  | TPrim TShort -> Llvm.i16_type ctx.gen_ctx
  | TPrim TInt -> Llvm.i32_type ctx.gen_ctx
  | TPrim TLong -> Llvm.i64_type ctx.gen_ctx
  | TPrim TFloat -> Llvm.float_type ctx.gen_ctx
  | TPrim TDouble -> Llvm.double_type ctx.gen_ctx
  | TPath path -> Hashtbl.find ctx.gen_structs path

let gen_const ctx = function
  | CInt i -> Llvm.const_int (gen_ty ctx (TPrim TInt)) i
  | CFloat f -> Llvm.const_float (gen_ty ctx (TPrim TFloat)) f
  | CString s -> Llvm.const_string ctx.gen_ctx s
  | CBool b -> Llvm.const_int (gen_ty ctx (TPrim TBool)) (if b then 1 else 0)
  | CNull -> Llvm.const_null (void_type ctx.gen_ctx)

let uninit ctx =
  dump_module ctx.gen_mod ;
  dispose_module ctx.gen_mod ;
  dispose_context ctx.gen_ctx
