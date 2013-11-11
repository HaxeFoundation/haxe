(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Gencommon.ReflectionCFs
open Ast
open Common
open Gencommon
open Gencommon.SourceWriter
open Type
open Printf
open Option
open ExtString

let is_cs_basic_type t =
  match follow t with
    | TInst( { cl_path = (["haxe"], "Int32") }, [] )
    | TInst( { cl_path = (["haxe"], "Int64") }, [] )
    | TInst( { cl_path = ([], "Int") }, [] )
    | TAbstract ({ a_path = ([], "Int") },[])
    | TInst( { cl_path = ([], "Float") }, [] )
    | TAbstract ({ a_path = ([], "Float") },[])
    | TEnum( { e_path = ([], "Bool") }, [] )
    | TAbstract ({ a_path = ([], "Bool") },[]) ->
      true
    | TAbstract _ when like_float t ->
      true
    | TEnum(e, _) when not (Meta.has Meta.Class e.e_meta) -> true
    | TInst(cl, _) when Meta.has Meta.Struct cl.cl_meta -> true
    | _ -> false

(* see http://msdn.microsoft.com/en-us/library/2sk3x8a7(v=vs.71).aspx *)
let cs_binops =
  [Ast.OpAdd, "op_Addition";
  Ast.OpSub, "op_Subtraction";
  Ast.OpMult, "op_Multiply";
  Ast.OpDiv, "op_Division";
  Ast.OpMod, "op_Modulus";
  Ast.OpXor, "op_ExclusiveOr";
  Ast.OpOr, "op_BitwiseOr";
  Ast.OpAnd, "op_BitwiseAnd";
  Ast.OpBoolAnd, "op_LogicalAnd";
  Ast.OpBoolOr, "op_LogicalOr";
  Ast.OpAssign, "op_Assign";
  Ast.OpShl, "op_LeftShift";
  Ast.OpShr, "op_RightShift";
  Ast.OpShr, "op_SignedRightShift";
  Ast.OpUShr, "op_UnsignedRightShift";
  Ast.OpEq, "op_Equality";
  Ast.OpGt, "op_GreaterThan";
  Ast.OpLt, "op_LessThan";
  Ast.OpNotEq, "op_Inequality";
  Ast.OpGte, "op_GreaterThanOrEqual";
  Ast.OpLte, "op_LessThanOrEqual";
  Ast.OpAssignOp Ast.OpMult, "op_MultiplicationAssignment";
  Ast.OpAssignOp Ast.OpSub, "op_SubtractionAssignment";
  Ast.OpAssignOp Ast.OpXor, "op_ExclusiveOrAssignment";
  Ast.OpAssignOp Ast.OpShl, "op_LeftShiftAssignment";
  Ast.OpAssignOp Ast.OpMod, "op_ModulusAssignment";
  Ast.OpAssignOp Ast.OpAdd, "op_AdditionAssignment";
  Ast.OpAssignOp Ast.OpAnd, "op_BitwiseAndAssignment";
  Ast.OpAssignOp Ast.OpOr, "op_BitwiseOrAssignment";
  (* op_Comma *)
  Ast.OpAssignOp Ast.OpDiv, "op_DivisionAssignment";]

let cs_unops =
  [Ast.Decrement, "op_Decrement";
  Ast.Increment, "op_Increment";
  Ast.Not, "op_UnaryNegation";
  Ast.Neg, "op_UnaryMinus";
  Ast.NegBits, "op_OnesComplement"]

let binops_names = List.fold_left (fun acc (op,n) -> PMap.add n op acc) PMap.empty cs_binops
let unops_names = List.fold_left (fun acc (op,n) -> PMap.add n op acc) PMap.empty cs_unops

let get_item = "get_Item"
let set_item = "set_Item"

let is_tparam t =
  match follow t with
    | TInst( { cl_kind = KTypeParameter _ }, [] ) -> true
    | _ -> false

let rec is_int_float t =
  match follow t with
    | TInst( { cl_path = (["haxe"], "Int32") }, [] )
    | TInst( { cl_path = (["haxe"], "Int64") }, [] )
    | TInst( { cl_path = ([], "Int") }, [] )
    | TAbstract ({ a_path = ([], "Int") },[])
    | TInst( { cl_path = ([], "Float") }, [] )
    | TAbstract ({ a_path = ([], "Float") },[]) ->
      true
    | TAbstract _ when like_float t ->
      true
    | TInst( { cl_path = (["haxe"; "lang"], "Null") }, [t] ) -> is_int_float t
    | _ -> false

let rec is_null t =
  match t with
    | TInst( { cl_path = (["haxe"; "lang"], "Null") }, _ )
    | TType( { t_path = ([], "Null") }, _ ) -> true
    | TType( t, tl ) -> is_null (apply_params t.t_types tl t.t_type)
    | TMono r ->
      (match !r with
      | Some t -> is_null t
      | _ -> false)
    | TLazy f ->
      is_null (!f())
    | _ -> false

let parse_explicit_iface =
  let regex = Str.regexp "\\." in
  let parse_explicit_iface str =
    let split = Str.split regex str in
    let rec get_iface split pack =
      match split with
        | clname :: fn_name :: [] -> fn_name, (List.rev pack, clname)
        | pack_piece :: tl -> get_iface tl (pack_piece :: pack)
        | _ -> assert false
    in
    get_iface split []
  in parse_explicit_iface

let is_string t =
  match follow t with
    | TInst( { cl_path = ([], "String") }, [] ) -> true
    | _ -> false

(* ******************************************* *)
(* CSharpSpecificESynf *)
(* ******************************************* *)

(*

  Some CSharp-specific syntax filters that must run before ExpressionUnwrap

  dependencies:
    It must run before ExprUnwrap, as it may not return valid Expr/Statement expressions
    It must run before ClassInstance, as it will detect expressions that need unchanged TTypeExpr

*)
module CSharpSpecificESynf =
struct

  let name = "csharp_specific_e"

  let priority = solve_deps name [DBefore ExpressionUnwrap.priority; DBefore ClassInstance.priority; DAfter TryCatchWrapper.priority]

  let get_cl_from_t t =
    match follow t with
      | TInst(cl,_) -> cl
      | _ -> assert false

  let traverse gen runtime_cl =
    let basic = gen.gcon.basic in
    let uint = match get_type gen ([], "UInt") with | TTypeDecl t -> TType(t, []) | TAbstractDecl a -> TAbstract(a, []) | _ -> assert false in

    let is_var = alloc_var "__is__" t_dynamic in

    let rec run e =
      match e.eexpr with
        (* Std.is() *)
        | TCall(
            { eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "is" })) },
            [ obj; { eexpr = TTypeExpr(TClassDecl { cl_path = [], "Dynamic" } | TAbstractDecl { a_path = [], "Dynamic" }) }]
          ) ->
            Type.map_expr run e
        | TCall(
            { eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "is"}) ) },
            [ obj; { eexpr = TTypeExpr(md) }]
          ) ->
          let mk_is obj md =
            { e with eexpr = TCall( { eexpr = TLocal is_var; etype = t_dynamic; epos = e.epos }, [
              obj;
              { eexpr = TTypeExpr md; etype = t_dynamic (* this is after all a syntax filter *); epos = e.epos }
            ] ) }
          in
          let obj = run obj in
          (match follow_module follow md with
            | TClassDecl{ cl_path = ([], "Float") } ->
              (* on the special case of seeing if it is a Float, we need to test if both it is a float and if it is an Int *)
              let mk_is local =
                mk_paren {
                  eexpr = TBinop(Ast.OpBoolOr, mk_is local md, mk_is local (TClassDecl (get_cl_from_t basic.tint)));
                  etype = basic.tbool;
                  epos = e.epos
                }
              in

              let ret = match obj.eexpr with
                | TLocal(v) -> mk_is obj
                | _ ->
                  let var = mk_temp gen "is" obj.etype in
                  let added = { obj with eexpr = TVars([var, Some(obj)]); etype = basic.tvoid } in
                  let local = mk_local var obj.epos in
                  {
                    eexpr = TBlock([ added; mk_is local ]);
                    etype = basic.tbool;
                    epos = e.epos
                  }
              in
              ret
            | TClassDecl{ cl_path = ([], "Int") } ->
              {
                eexpr = TCall(
                  mk_static_field_access_infer runtime_cl "isInt" e.epos [],
                  [ obj ]
                );
                etype = basic.tbool;
                epos = e.epos
              }
            | _ ->
              mk_is obj md
          )
        (* end Std.is() *)

        | TBinop( Ast.OpUShr, e1, e2 ) ->
          mk_cast e.etype { e with eexpr = TBinop( Ast.OpShr, mk_cast uint (run e1), run e2 ) }

        | TBinop( Ast.OpAssignOp Ast.OpUShr, e1, e2 ) ->
          let mk_ushr local =
            { e with eexpr = TBinop(Ast.OpAssign, local, run { e with eexpr = TBinop(Ast.OpUShr, local, run e2) }) }
          in

          let mk_local obj =
            let var = mk_temp gen "opUshr" obj.etype in
            let added = { obj with eexpr = TVars([var, Some(obj)]); etype = basic.tvoid } in
            let local = mk_local var obj.epos in
            local, added
          in

          let e1 = run e1 in

          let ret = match e1.eexpr with
            | TField({ eexpr = TLocal _ }, _)
            | TField({ eexpr = TTypeExpr _ }, _)
            | TArray({ eexpr = TLocal _ }, _)
            | TLocal(_) ->
              mk_ushr e1
            | TField(fexpr, field) ->
              let local, added = mk_local fexpr in
              { e with eexpr = TBlock([ added; mk_ushr { e1 with eexpr = TField(local, field) }  ]); }
            | TArray(ea1, ea2) ->
              let local, added = mk_local ea1 in
              { e with eexpr = TBlock([ added; mk_ushr { e1 with eexpr = TArray(local, ea2) }  ]); }
            | _ -> (* invalid left-side expression *)
              assert false
          in

          ret

        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* CSharpSpecificSynf *)
(* ******************************************* *)

(*

  Some CSharp-specific syntax filters  that can run after ExprUnwrap

  dependencies:
    Runs after ExprUnwrap

*)

module CSharpSpecificSynf =
struct

  let name = "csharp_specific"

  let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority;  DAfter HardNullableSynf.priority ]

  let get_cl_from_t t =
    match follow t with
      | TInst(cl,_) -> cl
      | _ -> assert false

  let is_tparam t =
    match follow t with
      | TInst( { cl_kind = KTypeParameter _ }, _ ) -> true
      | _ -> false

  let traverse gen runtime_cl =
    let basic = gen.gcon.basic in
		let tchar = match ( get_type gen (["cs"], "Char16") ) with
			| TTypeDecl t -> TType(t,[])
			| TAbstractDecl a -> TAbstract(a,[])
			| _ -> assert false
		in
    let string_ext = get_cl ( get_type gen (["haxe";"lang"], "StringExt")) in

    let is_string t = match follow t with | TInst({ cl_path = ([], "String") }, []) -> true | _ -> false in

    let clstring = match basic.tstring with | TInst(cl,_) -> cl | _ -> assert false in

    let is_struct t = (* not basic type *)
      match follow t with
        | TInst(cl, _) when Meta.has Meta.Struct cl.cl_meta -> true
        | _ -> false
    in

    let is_cl t = match gen.greal_type t with | TInst ( { cl_path = (["System"], "Type") }, [] ) -> true | _ -> false in

    let rec run e =
      match e.eexpr with

        (* Std.int() *)
        | TCall(
            { eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "int" }) ) },
            [obj]
          ) ->
          run (mk_cast basic.tint obj)
        (* end Std.int() *)

        (* TODO: change cf_name *)
        | TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = "length" })) ->
          { e with eexpr = TField(run ef, FDynamic "Length") }
        | TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = "toLowerCase" })) ->
          { e with eexpr = TField(run ef, FDynamic "ToLower") }
        | TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = "toUpperCase" })) ->
          { e with eexpr = TField(run ef, FDynamic "ToUpper") }

        | TCall( { eexpr = TField(_, FStatic({ cl_path = [], "String" }, { cf_name = "fromCharCode" })) }, [cc] ) ->
          { e with eexpr = TNew(get_cl_from_t basic.tstring, [], [mk_cast tchar (run cc); mk_int gen 1 cc.epos]) }
        | TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = ("charAt" as field) })) }, args )
        | TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = ("charCodeAt" as field) })) }, args )
        | TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = ("indexOf" as field) })) }, args )
        | TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = ("lastIndexOf" as field) })) }, args )
        | TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = ("split" as field) })) }, args )
        | TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = ("substring" as field) })) }, args )
        | TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = ("substr" as field) })) }, args ) ->
          { e with eexpr = TCall(mk_static_field_access_infer string_ext field e.epos [], [run ef] @ (List.map run args)) }
        | TNew( { cl_path = ([], "String") }, [], [p] ) -> run p (* new String(myString) -> myString *)

        | TCast(expr, _) when is_int_float e.etype && not (is_int_float expr.etype) && not (is_null e.etype) ->
          let needs_cast = match gen.gfollow#run_f e.etype with
            | TInst _ -> false
            | _ -> true
          in

          let fun_name = if like_int e.etype then "toInt" else "toDouble" in

          let ret = {
            eexpr = TCall(
              mk_static_field_access_infer runtime_cl fun_name expr.epos [],
              [ run expr ]
            );
            etype = basic.tint;
            epos = expr.epos
          } in

          if needs_cast then mk_cast e.etype ret else ret
        | TCast(expr, _) when is_string e.etype ->
          { e with eexpr = TCall( mk_static_field_access_infer runtime_cl "toString" expr.epos [], [run expr] ) }
        | TBinop( (Ast.OpNotEq as op), e1, e2)
        | TBinop( (Ast.OpEq as op), e1, e2) when is_string e1.etype || is_string e2.etype ->
          let mk_ret e = match op with | Ast.OpNotEq -> { e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) } | _ -> e in
          mk_ret { e with
            eexpr = TCall({
              eexpr = TField(mk_classtype_access clstring e.epos, FDynamic "Equals");
              etype = TFun(["obj1",false,basic.tstring; "obj2",false,basic.tstring], basic.tbool);
              epos = e1.epos
            }, [ run e1; run e2 ])
          }

        | TCast(expr, _) when is_tparam e.etype ->
          let static = mk_static_field_access_infer (runtime_cl) "genericCast" e.epos [e.etype] in
          { e with eexpr = TCall(static, [mk_local (alloc_var "$type_param" e.etype) expr.epos; run expr]); }

        | TBinop( (Ast.OpNotEq as op), e1, e2)
        | TBinop( (Ast.OpEq as op), e1, e2) when is_struct e1.etype || is_struct e2.etype ->
          let mk_ret e = match op with | Ast.OpNotEq -> { e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) } | _ -> e in
          mk_ret { e with
            eexpr = TCall({
              eexpr = TField(run e1, FDynamic "Equals");
              etype = TFun(["obj1",false,t_dynamic;], basic.tbool);
              epos = e1.epos
            }, [ run e2 ])
          }

        | TBinop ( (Ast.OpEq as op), e1, e2 )
        | TBinop ( (Ast.OpNotEq as op), e1, e2 ) when is_cl e1.etype ->
          let static = mk_static_field_access_infer (runtime_cl) "typeEq" e.epos [] in
          let ret = { e with eexpr = TCall(static, [run e1; run e2]); } in
          if op = Ast.OpNotEq then
            { ret with eexpr = TUnop(Ast.Not, Ast.Prefix, ret) }
          else
            ret

        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* Type Parameters Handling *)
let handle_type_params gen ifaces base_generic =
  let basic = gen.gcon.basic in
    (*
      starting to set gtparam_cast.
    *)

    (* NativeArray: the most important. *)

    (*
      var new_arr = new NativeArray<TO_T>(old_arr.Length);
      var i = -1;
      while( i < old_arr.Length )
      {
        new_arr[i] = (TO_T) old_arr[i];
      }
    *)

    let native_arr_cl = get_cl ( get_type gen (["cs"], "NativeArray") ) in

    let get_narr_param t = match follow t with
      | TInst({ cl_path = (["cs"], "NativeArray") }, [param]) -> param
      | _ -> assert false
    in

    let gtparam_cast_native_array e to_t =
      let old_param = get_narr_param e.etype in
      let new_param = get_narr_param to_t in

      let new_v = mk_temp gen "new_arr" to_t in
      let i = mk_temp gen "i" basic.tint in
      let old_len = mk_field_access gen e "Length" e.epos in
      let obj_v = mk_temp gen "obj" t_dynamic in
      let block = [
        {
          eexpr = TVars(
          [
            new_v, Some( {
              eexpr = TNew(native_arr_cl, [new_param], [old_len] );
              etype = to_t;
              epos = e.epos
            } );
            i, Some( mk_int gen (-1) e.epos )
          ]);
          etype = basic.tvoid;
          epos = e.epos };
        {
          eexpr = TWhile(
            {
              eexpr = TBinop(
                Ast.OpLt,
                { eexpr = TUnop(Ast.Increment, Ast.Prefix, mk_local i e.epos); etype = basic.tint; epos = e.epos },
                old_len
              );
              etype = basic.tbool;
              epos = e.epos
            },
            { eexpr = TBlock [
              {
                eexpr = TVars([obj_v, Some (mk_cast t_dynamic { eexpr = TArray(e, mk_local i e.epos); etype = old_param; epos = e.epos })]);
                etype = basic.tvoid;
                epos = e.epos
              };
              {
                eexpr = TIf({
                  eexpr = TBinop(Ast.OpNotEq, mk_local obj_v e.epos, null e.etype e.epos);
                  etype = basic.tbool;
                  epos = e.epos
                },
                {
                  eexpr = TBinop(
                    Ast.OpAssign,
                    { eexpr = TArray(mk_local new_v e.epos, mk_local i e.epos); etype = new_param; epos = e.epos },
                    mk_cast new_param (mk_local obj_v e.epos)
                  );
                  etype = new_param;
                  epos = e.epos
                },
                None);
                etype = basic.tvoid;
                epos = e.epos
              }
            ]; etype = basic.tvoid; epos = e.epos },
            Ast.NormalWhile
          );
          etype = basic.tvoid;
          epos = e.epos;
        };
        mk_local new_v e.epos
      ] in
      { eexpr = TBlock(block); etype = to_t; epos = e.epos }
    in

    Hashtbl.add gen.gtparam_cast (["cs"], "NativeArray") gtparam_cast_native_array;
    (* end set gtparam_cast *)

  TypeParams.RealTypeParams.default_config gen (fun e t -> gen.gcon.warning ("Cannot cast to " ^ (debug_type t)) e.epos; mk_cast t e) ifaces base_generic

let connecting_string = "?" (* ? see list here http://www.fileformat.info/info/unicode/category/index.htm and here for C# http://msdn.microsoft.com/en-us/library/aa664670.aspx *)
let default_package = "cs" (* I'm having this separated as I'm still not happy with having a cs package. Maybe dotnet would be better? *)
let strict_mode = ref false (* strict mode is so we can check for unexpected information *)

(* reserved c# words *)
let reserved = let res = Hashtbl.create 120 in
  List.iter (fun lst -> Hashtbl.add res lst ("@" ^ lst)) ["abstract"; "as"; "base"; "bool"; "break"; "byte"; "case"; "catch"; "char"; "checked"; "class";
    "const"; "continue"; "decimal"; "default"; "delegate"; "do"; "double"; "else"; "enum"; "event"; "explicit";
    "extern"; "false"; "finally"; "fixed"; "float"; "for"; "foreach"; "goto"; "if"; "implicit"; "in"; "int";
    "interface"; "internal"; "is"; "lock"; "long"; "namespace"; "new"; "null"; "object"; "operator"; "out"; "override";
    "params"; "private"; "protected"; "public"; "readonly"; "ref"; "return"; "sbyte"; "sealed"; "short"; "sizeof";
    "stackalloc"; "static"; "string"; "struct"; "switch"; "this"; "throw"; "true"; "try"; "typeof"; "uint"; "ulong";
    "unchecked"; "unsafe"; "ushort"; "using"; "virtual"; "volatile"; "void"; "while"; "add"; "ascending"; "by"; "descending";
    "dynamic"; "equals"; "from"; "get"; "global"; "group"; "into"; "join"; "let"; "on"; "orderby"; "partial";
    "remove"; "select"; "set"; "value"; "var"; "where"; "yield"];
  res

let dynamic_anon = TAnon( { a_fields = PMap.empty; a_status = ref Closed } )

let rec get_class_modifiers meta cl_type cl_access cl_modifiers =
  match meta with
    | [] -> cl_type,cl_access,cl_modifiers
    | (Meta.Struct,[],_) :: meta -> get_class_modifiers meta "struct" cl_access cl_modifiers
    | (Meta.Protected,[],_) :: meta -> get_class_modifiers meta cl_type "protected" cl_modifiers
    | (Meta.Internal,[],_) :: meta -> get_class_modifiers meta cl_type "internal" cl_modifiers
    (* no abstract for now | (":abstract",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("abstract" :: cl_modifiers)
    | (":static",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("static" :: cl_modifiers) TODO: support those types *)
    | (Meta.Final,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("sealed" :: cl_modifiers)
    | (Meta.Unsafe,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("unsafe" :: cl_modifiers)
    | _ :: meta -> get_class_modifiers meta cl_type cl_access cl_modifiers

let rec get_fun_modifiers meta access modifiers =
  match meta with
    | [] -> access,modifiers
    | (Meta.Protected,[],_) :: meta -> get_fun_modifiers meta "protected" modifiers
    | (Meta.Internal,[],_) :: meta -> get_fun_modifiers meta "internal" modifiers
    | (Meta.ReadOnly,[],_) :: meta -> get_fun_modifiers meta access ("readonly" :: modifiers)
    | (Meta.Unsafe,[],_) :: meta -> get_fun_modifiers meta access ("unsafe" :: modifiers)
    | (Meta.Volatile,[],_) :: meta -> get_fun_modifiers meta access ("volatile" :: modifiers)
    | _ :: meta -> get_fun_modifiers meta access modifiers

(* this was the way I found to pass the generator context to be accessible across all functions here *)
(* so 'configure' is almost 'top-level' and will have all functions needed to make this work *)
let configure gen =
  let basic = gen.gcon.basic in

  let fn_cl = get_cl (get_type gen (["haxe";"lang"],"Function")) in

  let null_t = (get_cl (get_type gen (["haxe";"lang"],"Null")) ) in

  let runtime_cl = get_cl (get_type gen (["haxe";"lang"],"Runtime")) in

  let no_root = Common.defined gen.gcon Define.NoRoot in

  let change_clname n = n in

  let change_id name = try
			Hashtbl.find reserved name
		with | Not_found ->
			if String.starts_with name "get_" || String.starts_with name "set_" then
				"_" ^ name
			else
				name
	in

  let change_ns md = if no_root then
    function
      | [] when is_hxgen md -> ["haxe";"root"]
      | ns -> List.map change_id ns
  else List.map change_id in

  let change_field = change_id in

  let write_id w name = write w (change_id name) in

  let write_field w name = write w (change_field name) in

  gen.gfollow#add ~name:"follow_basic" (fun t -> match t with
      | TEnum ({ e_path = ([], "Bool") }, [])
      | TAbstract ({ a_path = ([], "Bool") },[])
      | TEnum ({ e_path = ([], "Void") }, [])
      | TAbstract ({ a_path = ([], "Void") },[])
      | TInst ({ cl_path = ([],"Float") },[])
      | TAbstract ({ a_path = ([],"Float") },[])
      | TInst ({ cl_path = ([],"Int") },[])
      | TAbstract ({ a_path = ([],"Int") },[])
      | TType ({ t_path = [],"UInt" },[])
      | TAbstract ({ a_path = [],"UInt" },[])
      | TType ({ t_path = ["haxe";"_Int64"], "NativeInt64" },[])
      | TAbstract ({ a_path = ["haxe";"_Int64"], "NativeInt64" },[])
      | TType ({ t_path = ["haxe";"_Int64"], "NativeUInt64" },[])
      | TAbstract ({ a_path = ["haxe";"_Int64"], "NativeUInt64" },[])
      | TType ({ t_path = ["cs"],"UInt64" },[])
      | TAbstract ({ a_path = ["cs"],"UInt64" },[])
      | TType ({ t_path = ["cs"],"UInt8" },[])
      | TAbstract ({ a_path = ["cs"],"UInt8" },[])
      | TType ({ t_path = ["cs"],"Int8" },[])
      | TAbstract ({ a_path = ["cs"],"Int8" },[])
      | TType ({ t_path = ["cs"],"Int16" },[])
      | TAbstract ({ a_path = ["cs"],"Int16" },[])
      | TType ({ t_path = ["cs"],"UInt16" },[])
      | TAbstract ({ a_path = ["cs"],"UInt16" },[])
      | TType ({ t_path = ["cs"],"Char16" },[])
      | TAbstract ({ a_path = ["cs"],"Char16" },[])
      | TType ({ t_path = ["cs"],"Ref" },_)
      | TAbstract ({ a_path = ["cs"],"Ref" },_)
      | TType ({ t_path = ["cs"],"Out" },_)
      | TAbstract ({ a_path = ["cs"],"Out" },_)
      | TType ({ t_path = [],"Single" },[])
      | TAbstract ({ a_path = [],"Single" },[]) -> Some t
      | TType ({ t_path = [],"Null" },[_]) -> Some t
      | TAbstract ({ a_impl = Some _ } as a, pl) ->
          Some (gen.gfollow#run_f ( Codegen.Abstract.get_underlying_type a pl) )
      | TAbstract( { a_path = ([], "EnumValue") }, _  )
      | TInst( { cl_path = ([], "EnumValue") }, _  ) -> Some t_dynamic
      | _ -> None);

  let module_s md =
    let path = (t_infos md).mt_path in
    match path with
    | ([], "String") -> "string"
    | ([], "Null") -> path_s (change_ns md ["haxe"; "lang"], change_clname "Null")
    | (ns,clname) -> path_s (change_ns md ns, change_clname clname)
  in

  let ifaces = Hashtbl.create 1 in

  let ti64 = match ( get_type gen (["haxe";"_Int64"], "NativeInt64") ) with | TTypeDecl t -> TType(t,[]) | TAbstractDecl a -> TAbstract(a,[]) | _ -> assert false in

  let ttype = get_cl ( get_type gen (["System"], "Type") ) in

  let has_tdyn tl =
    List.exists (fun t -> match follow t with
    | TDynamic _ | TMono _ -> true
    | _ -> false
  ) tl
  in

  let rec real_type t =
    let t = gen.gfollow#run_f t in
    let ret = match t with
      | TAbstract ({ a_impl = Some _ } as a, pl) ->
        real_type (Codegen.Abstract.get_underlying_type a pl)
      | TInst( { cl_path = (["haxe"], "Int32") }, [] ) -> gen.gcon.basic.tint
      | TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> ti64
      | TAbstract( { a_path = [],"Class" }, _ )
      | TAbstract( { a_path = [],"Enum" }, _ )
      | TInst( { cl_path = ([], "Class") }, _ )
      | TInst( { cl_path = ([], "Enum") }, _ ) -> TInst(ttype,[])
      | TEnum(_, [])
      | TInst(_, []) -> t
      | TInst(cl, params) when
        has_tdyn params &&
        Hashtbl.mem ifaces cl.cl_path ->
          TInst(Hashtbl.find ifaces cl.cl_path, [])
      | TEnum(e, params) ->
        TEnum(e, List.map (fun _ -> t_dynamic) params)
      | TInst(cl, params) when Meta.has Meta.Enum cl.cl_meta ->
        TInst(cl, List.map (fun _ -> t_dynamic) params)
      | TInst(cl, params) -> TInst(cl, change_param_type (TClassDecl cl) params)
      | TType({ t_path = ([], "Null") }, [t]) ->
        (*
          Null<> handling is a little tricky.
          It will only change to haxe.lang.Null<> when the actual type is non-nullable or a type parameter
          It works on cases such as Hash<T> returning Null<T> since cast_detect will invoke real_type at the original type,
          Null<T>, which will then return the type haxe.lang.Null<>
        *)
        (match real_type t with
          | TInst( { cl_kind = KTypeParameter _ }, _ ) -> TInst(null_t, [t])
          | _ when is_cs_basic_type t -> TInst(null_t, [t])
          | _ -> real_type t)
      | TAbstract _
      | TType _ -> t
      | TAnon (anon) when (match !(anon.a_status) with | Statics _ | EnumStatics _ | AbstractStatics _ -> true | _ -> false) -> t
      | TFun _ -> TInst(fn_cl,[])
      | _ -> t_dynamic
    in
    ret
  and

  (*
    On hxcs, the only type parameters allowed to be declared are the basic c# types.
    That's made like this to avoid casting problems when type parameters in this case
    add nothing to performance, since the memory layout is always the same.

    To avoid confusion between Generic<Dynamic> (which has a different meaning in hxcs AST),
    all those references are using dynamic_anon, which means Generic<{}>
  *)
  change_param_type md tl =
    let is_hxgeneric = (TypeParams.RealTypeParams.is_hxgeneric md) in
    let ret t = match is_hxgeneric, real_type t with
      | false, _ -> t
      (*
        Because Null<> types need a special compiler treatment for many operations (e.g. boxing/unboxing),
        Null<> type parameters will be transformed into Dynamic.
      *)
      | true, TInst ( { cl_path = (["haxe";"lang"], "Null") }, _ ) -> dynamic_anon
      | true, TInst ( { cl_kind = KTypeParameter _ }, _ ) -> t
      | true, TInst _
      | true, TEnum _
      | true, TAbstract _ when is_cs_basic_type t -> t
      | true, TDynamic _ -> t
      | true, _ -> dynamic_anon
    in
    if is_hxgeneric && List.exists (fun t -> match follow t with | TDynamic _ -> true | _ -> false) tl then
      List.map (fun _ -> t_dynamic) tl
    else
      List.map ret tl
  in

  let is_dynamic t = match real_type t with
    | TMono _ | TDynamic _
    | TInst({ cl_kind = KTypeParameter _ }, _) -> true
    | TAnon anon ->
      (match !(anon.a_status) with
        | EnumStatics _ | Statics _ -> false
        | _ -> true
      )
    | _ -> false
  in

  let rec t_s t =
    match real_type t with
      (* basic types *)
      | TEnum ({ e_path = ([], "Bool") }, [])
      | TAbstract ({ a_path = ([], "Bool") },[]) -> "bool"
      | TEnum ({ e_path = ([], "Void") }, [])
      | TAbstract ({ a_path = ([], "Void") },[]) -> "object"
      | TInst ({ cl_path = ([],"Float") },[])
      | TAbstract ({ a_path = ([],"Float") },[]) -> "double"
      | TInst ({ cl_path = ([],"Int") },[])
      | TAbstract ({ a_path = ([],"Int") },[]) -> "int"
      | TType ({ t_path = [],"UInt" },[])
      | TAbstract ({ a_path = [],"UInt" },[]) -> "uint"
      | TType ({ t_path = ["haxe";"_Int64"], "NativeInt64" },[])
      | TAbstract ({ a_path = ["haxe";"_Int64"], "NativeInt64" },[]) -> "long"
      | TType ({ t_path = ["haxe";"_Int64"], "NativeUInt64" },[])
      | TAbstract ({ a_path = ["haxe";"_Int64"], "NativeUInt64" },[]) -> "ulong"
      | TType ({ t_path = ["cs"],"UInt64" },[])
      | TAbstract ({ a_path = ["cs"],"UInt64" },[]) -> "ulong"
      | TType ({ t_path = ["cs"],"UInt8" },[])
      | TAbstract ({ a_path = ["cs"],"UInt8" },[]) -> "byte"
      | TType ({ t_path = ["cs"],"Int8" },[])
      | TAbstract ({ a_path = ["cs"],"Int8" },[]) -> "sbyte"
      | TType ({ t_path = ["cs"],"Int16" },[])
      | TAbstract ({ a_path = ["cs"],"Int16" },[]) -> "short"
      | TType ({ t_path = ["cs"],"UInt16" },[])
      | TAbstract ({ a_path = ["cs"],"UInt16" },[]) -> "ushort"
      | TType ({ t_path = ["cs"],"Char16" },[])
      | TAbstract ({ a_path = ["cs"],"Char16" },[]) -> "char"
      | TType ({ t_path = [],"Single" },[])
      | TAbstract ({ a_path = [],"Single" },[]) -> "float"
      | TInst ({ cl_path = ["haxe"],"Int32" },[])
      | TAbstract ({ a_path = ["haxe"],"Int32" },[]) -> "int"
      | TInst ({ cl_path = ["haxe"],"Int64" },[])
      | TAbstract ({ a_path = ["haxe"],"Int64" },[]) -> "long"
      | TInst ({ cl_path = ([], "Dynamic") },_)
      | TAbstract ({ a_path = ([], "Dynamic") },_) -> "object"
      | TType ({ t_path = ["cs"],"Out" },[t])
      | TAbstract ({ a_path = ["cs"],"Out" },[t])
      | TType ({ t_path = ["cs"],"Ref" },[t])
      | TAbstract ({ a_path = ["cs"],"Ref" },[t]) -> t_s t
      | TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
        let rec check_t_s t =
          match real_type t with
            | TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
              (check_t_s param) ^ "[]"
            | _ -> t_s (run_follow gen t)
        in
        (check_t_s param) ^ "[]"
      | TInst({ cl_path = (["cs"], "Pointer") },[t])
      | TAbstract({ a_path = (["cs"], "Pointer") },[t])->
        t_s t ^ "*"
      (* end of basic types *)
      | TInst ({ cl_kind = KTypeParameter _; cl_path=p }, []) -> snd p
      | TMono r -> (match !r with | None -> "object" | Some t -> t_s (run_follow gen t))
      | TInst ({ cl_path = [], "String" }, []) -> "string"
      | TEnum (e, params) -> ("global::" ^ (module_s (TEnumDecl e)))
      | TInst (cl, _ :: _) when Meta.has Meta.Enum cl.cl_meta ->
        "global::" ^ module_s (TClassDecl cl)
      | TInst (({ cl_path = p } as cl), params) -> (path_param_s (TClassDecl cl) p params)
      | TType (({ t_path = p } as t), params) -> (path_param_s (TTypeDecl t) p params)
      | TAnon (anon) ->
        (match !(anon.a_status) with
          | Statics _ | EnumStatics _ -> "System.Type"
          | _ -> "object")
      | TDynamic _ -> "object"
      | TAbstract(a,pl) when a.a_impl <> None ->
        t_s (Codegen.Abstract.get_underlying_type a pl)
      (* No Lazy type nor Function type made. That's because function types will be at this point be converted into other types *)
      | _ -> if !strict_mode then begin trace ("[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"); assert false end else "[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"

  and path_param_s md path params =
      match params with
        | [] -> "global::" ^ module_s md
        | _ -> sprintf "%s<%s>" ("global::" ^ module_s md) (String.concat ", " (List.map (fun t -> t_s t) (change_param_type md params)))
  in

  let rett_s t =
    match t with
      | TEnum ({e_path = ([], "Void")}, [])
      | TAbstract ({ a_path = ([], "Void") },[]) -> "void"
      | _ -> t_s t
  in

  let argt_s t =
    match t with
      | TType ({ t_path = (["cs"], "Ref") }, [t])
      | TAbstract ({ a_path = (["cs"], "Ref") },[t]) -> "ref " ^ t_s t
      | TType ({ t_path = (["cs"], "Out") }, [t])
      | TAbstract ({ a_path = (["cs"], "Out") },[t]) -> "out " ^ t_s t
      | _ -> t_s t
  in

  let escape ichar b =
    match ichar with
      | 92 (* \ *) -> Buffer.add_string b "\\\\"
      | 39 (* ' *) -> Buffer.add_string b "\\\'"
      | 34 -> Buffer.add_string b "\\\""
      | 13 (* \r *) -> Buffer.add_string b "\\r"
      | 10 (* \n *) -> Buffer.add_string b "\\n"
      | 9 (* \t *) -> Buffer.add_string b "\\t"
      | c when c < 32 || (c >= 127 && c <= 0xFFFF) -> Buffer.add_string b (Printf.sprintf "\\u%.4x" c)
      | c when c > 0xFFFF -> Buffer.add_string b (Printf.sprintf "\\U%.8x" c)
      | c -> Buffer.add_char b (Char.chr c)
  in

  let escape s =
    let b = Buffer.create 0 in
    (try
      UTF8.validate s;
      UTF8.iter (fun c -> escape (UChar.code c) b) s
    with
      UTF8.Malformed_code ->
        String.iter (fun c -> escape (Char.code c) b) s
    );
    Buffer.contents b
  in

  let has_semicolon e =
    match e.eexpr with
      | TBlock _ | TFor _ | TSwitch _ | TPatMatch _ | TTry _ | TIf _ -> false
      | TWhile (_,_,flag) when flag = Ast.NormalWhile -> false
      | _ -> true
  in

  let in_value = ref false in

  let rec md_s md =
    let md = follow_module (gen.gfollow#run_f) md in
    match md with
      | TClassDecl ({ cl_types = [] } as cl) ->
        t_s (TInst(cl,[]))
      | TClassDecl (cl) when not (is_hxgen md) ->
        t_s (TInst(cl,List.map (fun t -> t_dynamic) cl.cl_types))
      | TEnumDecl ({ e_types = [] } as e) ->
        t_s (TEnum(e,[]))
      | TEnumDecl (e) when not (is_hxgen md) ->
        t_s (TEnum(e,List.map (fun t -> t_dynamic) e.e_types))
      | TClassDecl cl ->
        t_s (TInst(cl,[]))
      | TEnumDecl e ->
        t_s (TEnum(e,[]))
      | TTypeDecl t ->
        t_s (TType(t, List.map (fun t -> t_dynamic) t.t_types))
      | TAbstractDecl a ->
        t_s (TAbstract(a, List.map(fun t -> t_dynamic) a.a_types))
  in

  let rec ensure_local e explain =
    match e.eexpr with
      | TLocal _ -> e
      | TCast(e,_)
      | TParenthesis e | TMeta(_,e) -> ensure_local e explain
      | _ -> gen.gcon.error ("This function argument " ^ explain ^ " must be a local variable.") e.epos; e
  in

  let is_pointer t = match follow t with
    | TInst({ cl_path = (["cs"], "Pointer") }, _)
    | TAbstract ({ a_path = (["cs"], "Pointer") },_) ->
        true
    | _ ->
        false in

  let last_line = ref (-1) in
  let line_directive =
    if Common.defined gen.gcon Define.RealPosition then
      fun w p -> ()
    else fun w p ->
      let cur_line = Lexer.get_error_line p in
      let is_relative_path = (String.sub p.pfile 0 1) = "." in
      let file = if is_relative_path then Common.get_full_path p.pfile else p.pfile in
      if cur_line <> ((!last_line)+1) then begin print w "#line %d \"%s\"" cur_line (Ast.s_escape file); newline w end;
      last_line := cur_line
  in

  let rec extract_tparams params el =
    match el with
      | ({ eexpr = TLocal({ v_name = "$type_param" }) } as tp) :: tl ->
        extract_tparams (tp.etype :: params) tl
      | _ -> (params, el)
  in

	let is_extern_prop t name = match field_access gen t name with
		| FClassField(_,_,decl,v,_,t,_) ->
			Type.is_extern_field v && decl.cl_extern && not (is_hxgen (TClassDecl decl))
		| _ -> false
	in

  let expr_s w e =
    last_line := -1;
    in_value := false;
    let rec expr_s w e =
      let was_in_value = !in_value in
      in_value := true;
      (match e.eexpr with
				| TCall( ({ eexpr = TField(ef,f) } as e), [] ) when String.starts_with (field_name f) "get_" ->
					let name = field_name f in
					let propname = String.sub name 4 (String.length name - 4) in
					if is_extern_prop (gen.greal_type ef.etype) propname then begin
						expr_s w ef;
						write w ".";
						write_field w propname
					end else
						do_call w e []
				| TCall( ({ eexpr = TField(ef,f) } as e), [v] ) when String.starts_with (field_name f) "set_" ->
					let name = field_name f in
					let propname = String.sub name 4 (String.length name - 4) in
					if is_extern_prop (gen.greal_type ef.etype) propname then begin
						write w "(";
						expr_s w ef;
						write w ".";
						write_field w propname;
						write w " = ";
						expr_s w v;
						write w ")"
					end else
						do_call w e [v]
        | TField (e, (FStatic(_, cf) | FInstance(_, cf))) when Meta.has Meta.Native cf.cf_meta ->
          let rec loop meta = match meta with
            | (Meta.Native, [EConst (String s), _],_) :: _ ->
              expr_s w e; write w "."; write_field w s
            | _ :: tl -> loop tl
            | [] -> expr_s w e; write w "."; write_field w (cf.cf_name)
          in
          loop cf.cf_meta
        | TConst c ->
          (match c with
            | TInt i32 ->
              write w (Int32.to_string i32);
              (*match real_type e.etype with
                | TType( { t_path = (["haxe";"_Int64"], "NativeInt64") }, [] ) -> write w "L";
                | _ -> ()
              *)
            | TFloat s ->
              write w s;
              (if String.get s (String.length s - 1) = '.' then write w "0");
              (*match real_type e.etype with
                | TType( { t_path = ([], "Single") }, [] ) -> write w "f"
                | _ -> ()
              *)
            | TString s ->
              write w "\"";
              write w (escape s);
              write w "\""
            | TBool b -> write w (if b then "true" else "false")
            | TNull ->
              write w "default(";
              write w (t_s e.etype);
              write w ")"
            | TThis -> write w "this"
            | TSuper -> write w "base")
        | TLocal { v_name = "__sbreak__" } -> write w "break"
        | TLocal { v_name = "__undefined__" } ->
          write w (t_s (TInst(runtime_cl, List.map (fun _ -> t_dynamic) runtime_cl.cl_types)));
          write w ".undefined";
        | TLocal { v_name = "__typeof__" } -> write w "typeof"
        | TLocal { v_name = "__sizeof__" } -> write w "sizeof"
        | TLocal var ->
          write_id w var.v_name
        | TField (_, FEnum(e, ef)) ->
          let s = ef.ef_name in
          print w "%s." ("global::" ^ module_s (TEnumDecl e)); write_field w s
        | TArray (e1, e2) ->
          expr_s w e1; write w "["; expr_s w e2; write w "]"
        | TBinop ((Ast.OpAssign as op), e1, e2)
        | TBinop ((Ast.OpAssignOp _ as op), e1, e2) ->
          expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2
        | TBinop (op, e1, e2) ->
          write w "( ";
          expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2;
          write w " )"
        | TField ({ eexpr = TTypeExpr mt }, s) ->
          (match mt with
            | TClassDecl { cl_path = (["haxe"], "Int64") } -> write w ("global::" ^ module_s mt)
            | TClassDecl { cl_path = (["haxe"], "Int32") } -> write w ("global::" ^ module_s mt)
            | TClassDecl { cl_interface = true } ->
                write w ("global::" ^ module_s mt);
                write w "__Statics_";
            | TClassDecl cl -> write w (t_s (TInst(cl, List.map (fun _ -> t_empty) cl.cl_types)))
            | TEnumDecl en -> write w (t_s (TEnum(en, List.map (fun _ -> t_empty) en.e_types)))
            | TTypeDecl td -> write w (t_s (gen.gfollow#run_f (TType(td, List.map (fun _ -> t_empty) td.t_types))))
            | TAbstractDecl a -> write w (t_s (TAbstract(a, List.map (fun _ -> t_empty) a.a_types)))
          );
          write w ".";
          write_field w (field_name s)
        | TField (e, s) ->
          expr_s w e; write w "."; write_field w (field_name s)
        | TTypeExpr mt ->
          (match mt with
            | TClassDecl { cl_path = (["haxe"], "Int64") } -> write w ("global::" ^ module_s mt)
            | TClassDecl { cl_path = (["haxe"], "Int32") } -> write w ("global::" ^ module_s mt)
            | TClassDecl cl -> write w (t_s (TInst(cl, List.map (fun _ -> t_dynamic) cl.cl_types)))
            | TEnumDecl en -> write w (t_s (TEnum(en, List.map (fun _ -> t_dynamic) en.e_types)))
            | TTypeDecl td -> write w (t_s (gen.gfollow#run_f (TType(td, List.map (fun _ -> t_dynamic) td.t_types))))
            | TAbstractDecl a -> write w (t_s (TAbstract(a, List.map (fun _ -> t_dynamic) a.a_types)))
          )
        | TParenthesis e ->
          write w "("; expr_s w e; write w ")"
        | TMeta (_,e) ->
            expr_s w e
        | TArrayDecl el ->
          print w "new %s" (t_s e.etype);
          write w "{";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            acc + 1
          ) 0 el);
          write w "}"
        | TCall ({ eexpr = TLocal( { v_name = "__is__" } ) }, [ expr; { eexpr = TTypeExpr(md) } ] ) ->
          write w "( ";
          expr_s w expr;
          write w " is ";
          write w (md_s md);
          write w " )"
        | TCall ({ eexpr = TLocal( { v_name = "__as__" } ) }, [ expr; { eexpr = TTypeExpr(md) } ] ) ->
          write w "( ";
          expr_s w expr;
          write w " as ";
          write w (md_s md);
          write w " )"
        | TCall ({ eexpr = TLocal( { v_name = "__as__" } ) }, expr :: _ ) ->
          write w "( ";
          expr_s w expr;
          write w " as ";
          write w (t_s e.etype);
          write w " )";
        | TCall ({ eexpr = TLocal( { v_name = "__cs__" } ) }, [ { eexpr = TConst(TString(s)) } ] ) ->
          write w s
        | TCall ({ eexpr = TLocal( { v_name = "__unsafe__" } ) }, [ e ] ) ->
          write w "unsafe";
          expr_s w (mk_block e)
        | TCall ({ eexpr = TLocal( { v_name = "__checked__" } ) }, [ e ] ) ->
          write w "checked";
          expr_s w (mk_block e)
        | TCall ({ eexpr = TLocal( { v_name = "__lock__" } ) }, [ eobj; eblock ] ) ->
          write w "lock(";
          expr_s w eobj;
          write w ")";
          expr_s w (mk_block eblock)
        | TCall ({ eexpr = TLocal( { v_name = "__fixed__" } ) }, [ e ] ) ->
          let first = ref true in
          let rec loop = function
            | ({ eexpr = TVars([v, Some({ eexpr = TCast( { eexpr = TCast(e, _) }, _) }) ]) } as expr) :: tl when is_pointer v.v_type ->
              (if !first then first := false);
              write w "fixed(";
              let vf = mk_temp gen "fixed" v.v_type in
              expr_s w { expr with eexpr = TVars([vf, Some e]) };
              write w ")";
              begin_block w;
              expr_s w { expr with eexpr = TVars([v, Some (mk_local vf expr.epos)]) };
              write w ";";
              loop tl;
              end_block w
            | el when not !first ->
              expr_s w { e with eexpr = TBlock el }
            | _ ->
              trace (debug_expr e);
              gen.gcon.error "Invalid 'fixed' keyword format" e.epos
          in
          (match e.eexpr with
            | TBlock bl -> loop bl
            | _ ->
              trace "not block";
              trace (debug_expr e);
              gen.gcon.error "Invalid 'fixed' keyword format" e.epos
          )
        | TCall ({ eexpr = TLocal( { v_name = "__addressOf__" } ) }, [ e ] ) ->
          let e = ensure_local e "for addressOf" in
          write w "&";
          expr_s w e
        | TCall ({ eexpr = TLocal( { v_name = "__valueOf__" } ) }, [ e ] ) ->
          write w "*(";
          expr_s w e;
          write w ")"
        | TCall ({ eexpr = TLocal( { v_name = "__goto__" } ) }, [ { eexpr = TConst(TInt v) } ] ) ->
          print w "goto label%ld" v
        | TCall ({ eexpr = TLocal( { v_name = "__label__" } ) }, [ { eexpr = TConst(TInt v) } ] ) ->
          print w "label%ld: {}" v
        | TCall ({ eexpr = TLocal( { v_name = "__rethrow__" } ) }, _) ->
          write w "throw"
        (* operator overloading handling *)
        | TCall({ eexpr = TField(ef, FInstance(cl,{ cf_name = "__get" })) }, [idx]) when not (is_hxgen (TClassDecl cl)) ->
          expr_s w { e with eexpr = TArray(ef, idx) }
        | TCall({ eexpr = TField(ef, FInstance(cl,{ cf_name = "__set" })) }, [idx; v]) when not (is_hxgen (TClassDecl cl)) ->
          expr_s w { e with eexpr = TBinop(Ast.OpAssign, { e with eexpr = TArray(ef, idx) }, v) }
        | TCall({ eexpr = TField(ef, FStatic(_,cf)) }, el) when PMap.mem cf.cf_name binops_names ->
          let _, elr = extract_tparams [] el in
          (match elr with
          | [e1;e2] ->
            expr_s w { e with eexpr = TBinop(PMap.find cf.cf_name binops_names, e1, e2) }
          | _ -> do_call w e el)
        | TCall({ eexpr = TField(ef, FStatic(_,cf)) }, el) when PMap.mem cf.cf_name unops_names ->
          (match extract_tparams [] el with
          | _, [e1] ->
            expr_s w { e with eexpr = TUnop(PMap.find cf.cf_name unops_names, Ast.Prefix,e1) }
          | _ -> do_call w e el)
        | TCall (e, el) ->
          do_call w e el
        | TNew (({ cl_path = (["cs"], "NativeArray") } as cl), params, [ size ]) ->
          let rec check_t_s t times =
            match real_type t with
              | TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
                (check_t_s param (times+1))
              | _ ->
                print w "new %s[" (t_s (run_follow gen t));
                expr_s w size;
                print w "]";
                let rec loop i =
                  if i <= 0 then () else (write w "[]"; loop (i-1))
                in
                loop (times - 1)
          in
          check_t_s (TInst(cl, params)) 0
        | TNew ({ cl_path = ([], "String") } as cl, [], el) ->
          write w "new ";
          write w (t_s (TInst(cl, [])));
          write w "(";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            acc + 1
          ) 0 el);
          write w ")"
        | TNew ({ cl_kind = KTypeParameter _ } as cl, params, el) ->
          print w "default(%s) /* This code should never be reached. It was produced by the use of @:generic on a new type parameter instance: %s */" (t_s (TInst(cl,params))) (path_param_s (TClassDecl cl) cl.cl_path params)
        | TNew (cl, params, el) ->
          write w "new ";
          write w (path_param_s (TClassDecl cl) cl.cl_path params);
          write w "(";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            acc + 1
          ) 0 el);
          write w ")"
        | TUnop ((Ast.Increment as op), flag, e)
        | TUnop ((Ast.Decrement as op), flag, e) ->
          (match flag with
            | Ast.Prefix -> write w ( " " ^ (Ast.s_unop op) ^ " " ); expr_s w e
            | Ast.Postfix -> expr_s w e; write w (Ast.s_unop op))
        | TUnop (op, flag, e) ->
          (match flag with
            | Ast.Prefix -> write w ( " " ^ (Ast.s_unop op) ^ " (" ); expr_s w e; write w ") "
            | Ast.Postfix -> write w "("; expr_s w e; write w (") " ^ Ast.s_unop op))
        | TVars (v_eop_l) ->
          ignore (List.fold_left (fun acc (var, eopt) ->
            (if acc <> 0 then write w ", ");
            print w "%s " (t_s var.v_type);
            write_id w var.v_name;
            (match eopt with
              | None ->
                write w " = ";
                expr_s w (null var.v_type e.epos)
              | Some e ->
                write w " = ";
                expr_s w e
            );
            acc + 1
          ) 0 v_eop_l);
        | TBlock [e] when was_in_value ->
          expr_s w e
        | TBlock el ->
          begin_block w;
          List.iter (fun e ->
            line_directive w e.epos;
            in_value := false;
            expr_s w e;
            (if has_semicolon e then write w ";");
            newline w
          ) el;
          end_block w
        | TIf (econd, e1, Some(eelse)) when was_in_value ->
          write w "( ";
          expr_s w (mk_paren econd);
          write w " ? ";
          expr_s w (mk_paren e1);
          write w " : ";
          expr_s w (mk_paren eelse);
          write w " )";
        | TIf (econd, e1, eelse) ->
          write w "if ";
          expr_s w (mk_paren econd);
          write w " ";
          in_value := false;
          expr_s w (mk_block e1);
          (match eelse with
            | None -> ()
            | Some e ->
              write w " else ";
              in_value := false;
              expr_s w (mk_block e)
          )
        | TWhile (econd, eblock, flag) ->
          (match flag with
            | Ast.NormalWhile ->
              write w "while ";
              expr_s w (mk_paren econd);
              write w "";
              in_value := false;
              expr_s w (mk_block eblock)
            | Ast.DoWhile ->
              write w "do ";
              in_value := false;
              expr_s w (mk_block eblock);
              write w "while ";
              in_value := true;
              expr_s w (mk_paren econd);
          )
        | TSwitch (econd, ele_l, default) ->
          write w "switch ";
          expr_s w (mk_paren econd);
          begin_block w;
          List.iter (fun (el, e) ->
            List.iter (fun e ->
              write w "case ";
              in_value := true;
              expr_s w e;
              write w ":";
            ) el;
            newline w;
            in_value := false;
            expr_s w (mk_block e);
            newline w;
            newline w
          ) ele_l;
          if is_some default then begin
            write w "default:";
            newline w;
            in_value := false;
            expr_s w (get default);
            newline w;
          end;
          end_block w
        | TTry (tryexpr, ve_l) ->
          write w "try ";
          in_value := false;
          expr_s w (mk_block tryexpr);
          List.iter (fun (var, e) ->
            print w "catch (%s %s)" (t_s var.v_type) (var.v_name);
            in_value := false;
            expr_s w (mk_block e);
            newline w
          ) ve_l
        | TReturn eopt ->
          write w "return ";
          if is_some eopt then expr_s w (get eopt)
        | TBreak -> write w "break"
        | TContinue -> write w "continue"
        | TThrow e ->
          write w "throw ";
          expr_s w e
        | TCast (e1,md_t) ->
          ((*match gen.gfollow#run_f e.etype with
            | TType({ t_path = ([], "UInt") }, []) ->
              write w "( unchecked ((uint) ";
              expr_s w e1;
              write w ") )"
            | _ ->*)
              (* FIXME I'm ignoring module type *)
              print w "((%s) (" (t_s e.etype);
              expr_s w e1;
              write w ") )"
          )
        | TFor (_,_,content) ->
          write w "[ for not supported ";
          expr_s w content;
          write w " ]";
          if !strict_mode then assert false
        | TObjectDecl _ -> write w "[ obj decl not supported ]"; if !strict_mode then assert false
        | TFunction _ -> write w "[ func decl not supported ]"; if !strict_mode then assert false
        | TPatMatch _ -> write w "[ match not supported ]"; if !strict_mode then assert false
        | TEnumParameter _ -> write w "[ enum parameter not supported ]"; if !strict_mode then assert false
    )
    and do_call w e el =
      let params, el = extract_tparams [] el in
      let params = List.rev params in

      expr_s w e;

      (match params with
        | [] -> ()
        | params ->
          let md = match e.eexpr with
            | TField(ef, _) -> t_to_md (run_follow gen ef.etype)
            | _ -> assert false
          in
          write w "<";
          ignore (List.fold_left (fun acc t ->
            (if acc <> 0 then write w ", ");
            write w (t_s t);
            acc + 1
          ) 0 (change_param_type md params));
          write w ">"
      );

      let rec loop acc elist tlist =
        match elist, tlist with
          | e :: etl, (_,_,t) :: ttl ->
            (if acc <> 0 then write w ", ");
            (match real_type t with
              | TType({ t_path = (["cs"], "Ref") }, _)
              | TAbstract ({ a_path = (["cs"], "Ref") },_) ->
                let e = ensure_local e "of type cs.Ref" in
                write w "ref ";
                expr_s w e
              | TType({ t_path = (["cs"], "Out") }, _)
              | TAbstract ({ a_path = (["cs"], "Out") },_) ->
                let e = ensure_local e "of type cs.Out" in
                write w "out ";
                expr_s w e
              | _ ->
                expr_s w e
            );
            loop (acc + 1) etl ttl
          | e :: etl, [] ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            loop (acc + 1) etl []
          | _ -> ()
      in
      write w "(";
      let ft = match follow e.etype with
        | TFun(args,_) -> args
        | _ -> []
      in

      loop 0 el ft;

      write w ")"
    in
    expr_s w e
  in

  let get_string_params cl_types =
    match cl_types with
      | [] ->
        ("","")
      | _ ->
        let params = sprintf "<%s>" (String.concat ", " (List.map (fun (_, tcl) -> match follow tcl with | TInst(cl, _) -> snd cl.cl_path | _ -> assert false) cl_types)) in
        let params_extends = List.fold_left (fun acc (name, t) ->
          match run_follow gen t with
            | TInst (cl, p) ->
              (match cl.cl_implements with
                | [] -> acc
                | _ -> acc) (* TODO
                | _ -> (sprintf " where %s : %s" name (String.concat ", " (List.map (fun (cl,p) -> path_param_s (TClassDecl cl) cl.cl_path p) cl.cl_implements))) :: acc ) *)
            | _ -> trace (t_s t); assert false (* FIXME it seems that a cl_types will never be anything other than cl.cl_types. I'll take the risk and fail if not, just to see if that confirms *)
        ) [] cl_types in
        (params, String.concat " " params_extends)
  in

	let rec gen_prop w is_static cl is_final (prop,t,get,set) =
    let is_interface = cl.cl_interface in
		let fn_is_final = function
			| None -> true
			| Some ({ cf_kind = Method mkind } as m) ->
				(match mkind with | MethInline -> true | _ -> false) || Meta.has Meta.Final m.cf_meta
			| _ -> assert false
		in
		let is_virtual = not (is_final || Meta.has Meta.Final prop.cf_meta || fn_is_final get || fn_is_final set) in

		let fn_is_override = function
			| Some cf -> List.memq cf cl.cl_overrides
			| None -> false
		in
		let is_override = fn_is_override get || fn_is_override set in
		let visibility = if is_interface then "" else "public" in
		let visibility, modifiers = get_fun_modifiers prop.cf_meta visibility [] in
		let v_n = if is_static then "static " else if is_override && not is_interface then "override " else if is_virtual then "virtual " else "" in
		print w "%s %s %s %s %s" (visibility) v_n (String.concat " " modifiers) (t_s (run_follow gen t)) (change_field prop.cf_name);
		let check cf = match cf with
			| Some ({ cf_overloads = o :: _ } as cf) ->
					gen.gcon.error "Property functions with more than one overload is currently unsupported" cf.cf_pos;
					gen.gcon.error "Property functions with more than one overload is currently unsupported" o.cf_pos
			| _ -> ()
		in
		check get;
		check set;

		begin_block w;
		(match prop.cf_kind with
		| Var { v_read = AccCall } when is_interface ->
			write w "get;";
		| _ -> match get with
			| Some _  ->
				print w "get { return _get_%s(); }" prop.cf_name;
				newline w
			| _ -> ());
		(match prop.cf_kind with
		| Var { v_write = AccCall } when is_interface ->
			write w "set;";
		| _ -> match set with
			| Some _  ->
				print w "set { _set_%s(value); }" prop.cf_name;
				newline w
			| _ -> ());
		end_block w;
	in

  let rec gen_class_field w ?(is_overload=false) is_static cl is_final cf =
    let is_interface = cl.cl_interface in
    let name, is_new, is_explicit_iface = match cf.cf_name with
      | "new" -> snd cl.cl_path, true, false
      | name when String.contains name '.' ->
        let fn_name, path = parse_explicit_iface name in
        (path_s path) ^ "." ^ fn_name, false, true
      | name -> try
        let binop = PMap.find name binops_names in
        "operator " ^ s_binop binop, false, false
      with | Not_found -> try
        let unop = PMap.find name unops_names in
        "operator " ^ s_unop unop, false, false
      with | Not_found ->
        name, false, false
    in
    let rec loop_static cl =
      match is_static, cl.cl_super with
        | false, _ -> []
        | true, None -> []
        | true, Some(cl,_) ->
           (try
              let cf2 = PMap.find cf.cf_name cl.cl_statics in
              Gencommon.CastDetect.type_eq gen EqStrict cf.cf_type cf2.cf_type;
              ["new"]
            with
              | Not_found | Unify_error _ ->
                  loop_static cl
            )
    in
    let modf = loop_static cl in

    (match cf.cf_kind with
      | Var _
      | Method (MethDynamic) when not (Type.is_extern_field cf) ->
        (if is_overload || List.exists (fun cf -> cf.cf_expr <> None) cf.cf_overloads then
          gen.gcon.error "Only normal (non-dynamic) methods can be overloaded" cf.cf_pos);
        if not is_interface then begin
          let access, modifiers = get_fun_modifiers cf.cf_meta "public" [] in
          let modifiers = modifiers @ modf in
          (match cf.cf_expr with
            | Some e ->
              print w "%s %s%s %s %s = " access (if is_static then "static " else "") (String.concat " " modifiers) (t_s (run_follow gen cf.cf_type)) (change_field name);
              expr_s w e;
              write w ";"
            | None ->
              print w "%s %s%s %s %s;" access (if is_static then "static " else "") (String.concat " " modifiers) (t_s (run_follow gen cf.cf_type)) (change_field name)
          )
        end (* TODO see how (get,set) variable handle when they are interfaces *)
      | Method _ when Type.is_extern_field cf || (match cl.cl_kind, cf.cf_expr with | KAbstractImpl _, None -> true | _ -> false) ->
        List.iter (fun cf -> if cl.cl_interface || cf.cf_expr <> None then
          gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
        ) cf.cf_overloads
      | Var _ | Method MethDynamic -> ()
      | Method mkind ->
        List.iter (fun cf ->
          if cl.cl_interface || cf.cf_expr <> None then
            gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
        ) cf.cf_overloads;
        let is_virtual = not is_final && match mkind with | MethInline -> false | _ when not is_new -> true | _ -> false in
        let is_virtual = if not is_virtual || Meta.has Meta.Final cf.cf_meta then false else is_virtual in
        let is_override = List.memq cf cl.cl_overrides in
        let is_override = is_override || match cf.cf_name, follow cf.cf_type with
          | "Equals", TFun([_,_,targ], tret) ->
            (match follow targ, follow tret with
              | TDynamic _, TEnum({ e_path = ([], "Bool") }, [])
              | TDynamic _, TAbstract({ a_path = ([], "Bool") }, []) -> true
              | _ -> false)
          | "GetHashCode", TFun([],_) -> true
          | _ -> false
        in

        let is_virtual = is_virtual && not (Meta.has Meta.Final cl.cl_meta) && not (is_interface) in
        let visibility = if is_interface then "" else "public" in

        let visibility, modifiers = get_fun_modifiers cf.cf_meta visibility [] in
        let modifiers = modifiers @ modf in
        let visibility, is_virtual = if is_explicit_iface then "",false else visibility, is_virtual in
        let v_n = if is_static then "static " else if is_override && not is_interface then "override " else if is_virtual then "virtual " else "" in
        let cf_type = if is_override && not is_overload && not (Meta.has Meta.Overload cf.cf_meta) then match field_access gen (TInst(cl, List.map snd cl.cl_types)) cf.cf_name with | FClassField(_,_,_,_,_,actual_t,_) -> actual_t | _ -> assert false else cf.cf_type in
        let ret_type, args = match follow cf_type with | TFun (strbtl, t) -> (t, strbtl) | _ -> assert false in

        (* public static void funcName *)
        print w "%s %s %s %s %s" (visibility) v_n (String.concat " " modifiers) (if is_new then "" else rett_s (run_follow gen ret_type)) (change_field name);
        let params, params_ext = get_string_params cf.cf_params in
        (* <T>(string arg1, object arg2) with T : object *)
        (match cf.cf_expr with
        | Some { eexpr = TFunction tf } ->
            print w "%s(%s)%s" (params) (String.concat ", " (List.map2 (fun (var, _) (_,_,t) -> sprintf "%s %s" (argt_s (run_follow gen t)) (change_id var.v_name)) tf.tf_args args)) (params_ext)
        | _ ->
            print w "%s(%s)%s" (params) (String.concat ", " (List.map (fun (name, _, t) -> sprintf "%s %s" (argt_s (run_follow gen t)) (change_id name)) args)) (params_ext)
        );
        if is_interface then
          write w ";"
        else begin
          let rec loop meta =
            match meta with
              | [] ->
                let expr = match cf.cf_expr with
                  | None -> mk (TBlock([])) t_dynamic Ast.null_pos
                  | Some s ->
                    match s.eexpr with
                      | TFunction tf ->
                        mk_block (tf.tf_expr)
                      | _ -> assert false (* FIXME *)
                in
                (if is_new then begin
                  let rec get_super_call el =
                    match el with
                      | ( { eexpr = TCall( { eexpr = TConst(TSuper) }, _) } as call) :: rest ->
                        Some call, rest
                      | ( { eexpr = TBlock(bl) } as block ) :: rest ->
                        let ret, mapped = get_super_call bl in
                        ret, ( { block with eexpr = TBlock(mapped) } :: rest )
                      | _ ->
                        None, el
                  in
                  match expr.eexpr with
                    | TBlock(bl) ->
                      let super_call, rest = get_super_call bl in
                      (match super_call with
                        | None -> ()
                        | Some sc ->
                          write w " : ";
                          let t = Common.timer "expression to string" in
                          expr_s w sc;
                          t()
                      );
                      begin_block w;
                      write w "unchecked ";
                      let t = Common.timer "expression to string" in
                      expr_s w { expr with eexpr = TBlock(rest) };
                      t();
                      write w "#line default";
                      end_block w;
                    | _ -> assert false
                end else begin
                  begin_block w;
                  write w "unchecked ";
                  let t = Common.timer "expression to string" in
                  expr_s w expr;
                  t();
                  write w "#line default";
                  end_block w;
                end)
              | (Meta.FunctionCode, [Ast.EConst (Ast.String contents),_],_) :: tl ->
                begin_block w;
                write w contents;
                end_block w
              | _ :: tl -> loop tl
          in
          loop cf.cf_meta

        end);
      newline w;
      newline w;
  in

  let check_special_behaviors w cl = match cl.cl_kind with
  | KAbstractImpl _ -> ()
  | _ ->
    (* get/set pairs *)
    let pairs = ref PMap.empty in
    (try
      let get = PMap.find "__get" cl.cl_fields in
      List.iter (fun cf ->
        let args,ret = get_fun cf.cf_type in
        match args with
        | [_,_,idx] -> pairs := PMap.add (t_s idx) ( t_s ret, Some cf, None ) !pairs
        | _ -> gen.gcon.warning "The __get function must have exactly one argument (the index)" cf.cf_pos
      ) (get :: get.cf_overloads)
    with | Not_found -> ());
    (try
      let set = PMap.find "__set" cl.cl_fields in
      List.iter (fun cf ->
        let args, ret = get_fun cf.cf_type in
        match args with
        | [_,_,idx; _,_,v] -> (try
          let vt, g, _ = PMap.find (t_s idx) !pairs in
          let tvt = t_s v in
          if vt <> tvt then gen.gcon.warning "The __get function of same index has a different type from this __set function" cf.cf_pos;
          pairs := PMap.add (t_s idx) (vt, g, Some cf) !pairs
        with | Not_found ->
          pairs := PMap.add (t_s idx) (t_s v, None, Some cf) !pairs)
        | _ ->
          gen.gcon.warning "The __set function must have exactly two arguments (index, value)" cf.cf_pos
      ) (set :: set.cf_overloads)
    with | Not_found -> ());
    PMap.iter (fun idx (v, get, set) ->
      print w "public %s this[%s index]" v idx;
        begin_block w;
        (match get with
        | None -> ()
        | Some _ ->
          write w "get";
          begin_block w;
          write w "return this.__get(index);";
          end_block w);
        (match set with
        | None -> ()
        | Some _ ->
          write w "set";
          begin_block w;
          write w "this.__set(index,value);";
          end_block w);
        end_block w) !pairs;
    (if not (PMap.is_empty !pairs) then try
      let get = PMap.find "__get" cl.cl_fields in
      let idx_t, v_t = match follow get.cf_type with
        | TFun([_,_,arg_t],ret_t) ->
          t_s (run_follow gen arg_t), t_s (run_follow gen ret_t)
        | _ -> gen.gcon.error "The __get function must be a function with one argument. " get.cf_pos; assert false
      in
      List.iter (fun (cl,args) ->
        match cl.cl_array_access with
          | None -> ()
          | Some t ->
            let changed_t = apply_params cl.cl_types (List.map (fun _ -> t_dynamic) cl.cl_types) t in
            let t_as_s = t_s (run_follow gen changed_t) in
            print w "%s %s.this[int key]" t_as_s (t_s (TInst(cl, args)));
              begin_block w;
              write w "get";
              begin_block w;
                print w "return ((%s) this.__get(key));" t_as_s;
              end_block w;
              write w "set";
              begin_block w;
                print w "this.__set(key, (%s) value);" v_t;
              end_block w;
            end_block w;
            newline w;
            newline w
      ) cl.cl_implements
    with | Not_found -> ());
    if cl.cl_interface && is_hxgen (TClassDecl cl) && is_some cl.cl_array_access then begin
      let changed_t = apply_params cl.cl_types (List.map (fun _ -> t_dynamic) cl.cl_types) (get cl.cl_array_access) in
      print w "%s this[int key]" (t_s (run_follow gen changed_t));
      begin_block w;
        write w "get;";
        newline w;
        write w "set;";
        newline w;
      end_block w;
      newline w;
      newline w
    end;
    (try
      if cl.cl_interface then raise Not_found;
      let cf = PMap.find "toString" cl.cl_fields in
      (if List.exists (fun c -> c.cf_name = "toString") cl.cl_overrides then raise Not_found);
      (match cf.cf_type with
        | TFun([], ret) ->
          (match real_type ret with
            | TInst( { cl_path = ([], "String") }, []) ->
              write w "public override string ToString()";
              begin_block w;
              write w "return this.toString();";
              end_block w;
              newline w;
              newline w
            | _ ->
              gen.gcon.error "A toString() function should return a String!" cf.cf_pos
          )
        | _ -> ()
      )
    with | Not_found -> ());
    (* properties *
    let handle_prop static f =
      match f.cf_kind with
      | Method _ -> ()
      | Var v when not (Type.is_extern_field f) -> ()
      | Var v ->
        let prop acc = match acc with
          | AccNo | AccNever | AccCall -> true
          | _ -> false
        in
        if prop v.v_read && prop v.v_write && (v.v_read = AccCall || v.v_write = AccCall) then begin
          let this = if static then
            mk_classtype_access cl f.cf_pos
          else
            { eexpr = TConst TThis; etype = TInst(cl,List.map snd cl.cl_types); epos = f.cf_pos }
          in
          print w "public %s%s %s" (if static then "static " else "") (t_s f.cf_type) f.cf_name;
          begin_block w;
          (match v.v_read with
          | AccCall ->
            write w "get";
            begin_block w;
            write w "return ";
            expr_s w this;
            print w "._get_%s();" f.cf_name;
            end_block w
          | _ -> ());
          (match v.v_write with
          | AccCall ->
            write w "set";
            begin_block w;
            expr_s w this;
            print w "._set_%s(value);" f.cf_name;
            end_block w
          | _ -> ());
          end_block w;
        end
    in
    List.iter (handle_prop true) cl.cl_ordered_statics;
    List.iter (handle_prop false) cl.cl_ordered_fields*)
  in

  let gen_class w cl =
    write w "#pragma warning disable 109, 114, 219, 429, 168, 162";
    newline w;
    let should_close = match change_ns (TClassDecl cl) (fst (cl.cl_path)) with
      | [] -> false
      | ns ->
        print w "namespace %s" (String.concat "." ns);
        begin_block w;
        true
    in

    let is_main =
      match gen.gcon.main_class with
        | Some ( (_,"Main") as path) when path = cl.cl_path && not cl.cl_interface ->
          (*
            for cases where the main class is called Main, there will be a problem with creating the entry point there.
            In this special case, a special entry point class will be created
          *)
          write w "public class EntryPoint__Main";
          begin_block w;
          write w "public static void Main()";
          begin_block w;
          (if Hashtbl.mem gen.gtypes (["cs"], "Boot") then write w "global::cs.Boot.init();"; newline w);
          expr_s w { eexpr = TTypeExpr(TClassDecl cl); etype = t_dynamic; epos = Ast.null_pos };
          write w ".main();";
          end_block w;
          end_block w;
          false
        | Some path when path = cl.cl_path && not cl.cl_interface -> true
        | _ -> false
    in

    let clt, access, modifiers = get_class_modifiers cl.cl_meta (if cl.cl_interface then "interface" else "class") "public" [] in
    let is_final = clt = "struct" || Meta.has Meta.Final cl.cl_meta in

    print w "%s %s %s %s" access (String.concat " " modifiers) clt (change_clname (snd cl.cl_path));
    (* type parameters *)
    let params, params_ext = get_string_params cl.cl_types in
    let extends_implements = (match cl.cl_super with | None -> [] | Some (cl,p) -> [path_param_s (TClassDecl cl) cl.cl_path p]) @ (List.map (fun (cl,p) -> path_param_s (TClassDecl cl) cl.cl_path p) cl.cl_implements) in
    (match extends_implements with
      | [] -> print w "%s %s" params params_ext
      | _ -> print w "%s : %s %s" params (String.concat ", " extends_implements) params_ext);
    (* class head ok: *)
    (* public class Test<A> : X, Y, Z where A : Y *)
    begin_block w;
    (* our constructor is expected to be a normal "new" function *
    if !strict_mode && is_some cl.cl_constructor then assert false;*)

    let rec loop meta =
      match meta with
        | [] ->  ()
        | (Meta.ClassCode, [Ast.EConst (Ast.String contents),_],_) :: tl ->
          write w contents
        | _ :: tl -> loop tl
    in
    loop cl.cl_meta;

    if is_main then begin
      write w "public static void Main()";
      begin_block w;
      (if Hashtbl.mem gen.gtypes (["cs"], "Boot") then write w "global::cs.Boot.init();"; newline w);
      write w "main();";
      end_block w
    end;

    (match cl.cl_init with
      | None -> ()
      | Some init ->
        print w "static %s() " (snd cl.cl_path);
        expr_s w (mk_block init));

		(* collect properties *)
		let partition_props cl cflist =
			let t = TInst(cl, List.map snd cl.cl_types) in
			(* first get all vars declared as properties *)
			let props, nonprops = List.partition (fun v -> match v.cf_kind with
				| Var { v_read = AccCall } | Var { v_write = AccCall } ->
					Type.is_extern_field v
				| _ -> false
			) cflist in
			let props = ref (List.map (fun v -> (v.cf_name, ref (v,v.cf_type,None,None))) props) in

			let find_prop name = try
					List.assoc name !props
				with | Not_found -> match field_access gen t name with
					| FClassField (_,_,decl,v,_,t,_) when Type.is_extern_field v && decl.cl_extern && not (is_hxgen (TClassDecl decl)) ->
						let ret = ref (v,t,None,None) in
						props := (name, ret) :: !props;
						ret
					| _ -> raise Not_found
			in
			(* get all functions that are getters/setters *)
			List.iter (function
				| cf when String.starts_with cf.cf_name "get_" -> (try
					(* find the property *)
					let prop = find_prop (String.sub cf.cf_name 4 (String.length cf.cf_name - 4)) in
					let v, t, get, set = !prop in
					assert (get = None);
					prop := (v,t,Some cf,set);
				with | Not_found -> ())
				| cf when String.starts_with cf.cf_name "set_" -> (try
					(* find the property *)
					let prop = find_prop (String.sub cf.cf_name 4 (String.length cf.cf_name - 4)) in
					let v, t, get, set = !prop in
					assert (set = None);
					prop := (v,t,get,Some cf);
				with | Not_found -> ())
				| _ -> ()
			) nonprops;
			let ret = List.map (fun (_,v) -> !v) !props in
			let ret = List.filter (function | (_,_,None,None) -> false | _ -> true) ret in
			ret, nonprops
		in

		let fprops, fnonprops = partition_props cl cl.cl_ordered_fields in
		let sprops, snonprops = partition_props cl cl.cl_ordered_statics in
    (if is_some cl.cl_constructor then gen_class_field w false cl is_final (get cl.cl_constructor));
		if not cl.cl_interface then begin
			List.iter (gen_class_field w true cl is_final) snonprops;
			List.iter (gen_prop w true cl is_final) sprops
		end;
    List.iter (gen_class_field w false cl is_final) fnonprops;
		List.iter (gen_prop w false cl is_final) fprops;
    check_special_behaviors w cl;
    end_block w;
    if cl.cl_interface && cl.cl_ordered_statics <> [] then begin
      print w "public class %s__Statics_" (snd cl.cl_path);
      begin_block w;
      List.iter (gen_class_field w true { cl with cl_interface = false } is_final) cl.cl_ordered_statics;
      end_block w
    end;
    if should_close then end_block w
  in


  let gen_enum w e =
    let should_close = match change_ns (TEnumDecl e) (fst e.e_path) with
      | [] -> false
      | ns ->
        print w "namespace %s" (String.concat "." ns);
        begin_block w;
        true
    in

    print w "public enum %s" (change_clname (snd e.e_path));
    begin_block w;
    write w (String.concat ", " (List.map (change_id) e.e_names));
    end_block w;

    if should_close then end_block w
  in

  let module_type_gen w md_tp =
    match md_tp with
      | TClassDecl cl ->
        if not cl.cl_extern then begin
          (if no_root && len w = 0 then write w "using haxe.root;"; newline w;);
          gen_class w cl;
          newline w;
          newline w
        end;
        (not cl.cl_extern)
      | TEnumDecl e ->
        if not e.e_extern then begin
          (if no_root && len w = 0 then write w "using haxe.root;"; newline w;);
          gen_enum w e;
          newline w;
          newline w
        end;
        (not e.e_extern)
      | TAbstractDecl _
      | TTypeDecl _ ->
        false
  in

  let module_gen w md_def =
    List.fold_left (fun should md -> module_type_gen w md or should) false md_def.m_types
  in

  (* generate source code *)
  init_ctx gen;

  Hashtbl.add gen.gspecial_vars "__rethrow__" true;
  Hashtbl.add gen.gspecial_vars "__typeof__" true;
  Hashtbl.add gen.gspecial_vars "__label__" true;
  Hashtbl.add gen.gspecial_vars "__goto__" true;
  Hashtbl.add gen.gspecial_vars "__is__" true;
  Hashtbl.add gen.gspecial_vars "__as__" true;
  Hashtbl.add gen.gspecial_vars "__cs__" true;

  Hashtbl.add gen.gspecial_vars "__checked__" true;
  Hashtbl.add gen.gspecial_vars "__lock__" true;
  Hashtbl.add gen.gspecial_vars "__fixed__" true;
  Hashtbl.add gen.gspecial_vars "__unsafe__" true;
  Hashtbl.add gen.gspecial_vars "__addressOf__" true;
  Hashtbl.add gen.gspecial_vars "__valueOf__" true;
  Hashtbl.add gen.gspecial_vars "__sizeof__" true;

  Hashtbl.add gen.gsupported_conversions (["haxe"; "lang"], "Null") (fun t1 t2 -> true);
  let last_needs_box = gen.gneeds_box in
  gen.gneeds_box <- (fun t -> match t with | TInst( { cl_path = (["haxe"; "lang"], "Null") }, _ ) -> true | _ -> last_needs_box t);

  gen.greal_type <- real_type;
  gen.greal_type_param <- change_param_type;

  SetHXGen.run_filter gen SetHXGen.default_hxgen_func;

  (* before running the filters, follow all possible types *)
  (* this is needed so our module transformations don't break some core features *)
  (* like multitype selection *)
  let run_follow_gen = run_follow gen in
  let rec type_map e = Type.map_expr_type (fun e->type_map e) (run_follow_gen)  (fun tvar-> tvar.v_type <- (run_follow_gen tvar.v_type); tvar) e in
  let super_map (cl,tl) = (cl, List.map run_follow_gen tl) in
  List.iter (function
    | TClassDecl cl ->
        let all_fields = (Option.map_default (fun cf -> [cf]) [] cl.cl_constructor) @ cl.cl_ordered_fields @ cl.cl_ordered_statics in
        List.iter (fun cf ->
          cf.cf_type <- run_follow_gen cf.cf_type;
          cf.cf_expr <- Option.map type_map cf.cf_expr
        ) all_fields;
       cl.cl_dynamic <- Option.map run_follow_gen cl.cl_dynamic;
       cl.cl_array_access <- Option.map run_follow_gen cl.cl_array_access;
       cl.cl_init <- Option.map type_map cl.cl_init;
       cl.cl_super <- Option.map super_map cl.cl_super;
       cl.cl_implements <- List.map super_map cl.cl_implements
    | _ -> ()
    ) gen.gcon.types;

  let closure_t = ClosuresToClass.DoubleAndDynamicClosureImpl.get_ctx gen 6 in

  (*let closure_t = ClosuresToClass.create gen 10 float_cl
    (fun l -> l)
    (fun l -> l)
    (fun args -> args)
    (fun args -> [])
  in
  ClosuresToClass.configure gen (ClosuresToClass.default_implementation closure_t (fun e _ _ -> e));

  StubClosureImpl.configure gen (StubClosureImpl.default_implementation gen float_cl 10 (fun e _ _ -> e));*)

  let tp_v = alloc_var "$type_param" t_dynamic in
  let mk_tp t pos = { eexpr = TLocal(tp_v); etype = t; epos = pos } in
  TypeParams.configure gen (fun ecall efield params elist ->
    match efield.eexpr with
    | TField(_, FEnum _) ->
        { ecall with eexpr = TCall(efield, elist) }
    | _ ->
        { ecall with eexpr = TCall(efield, (List.map (fun t -> mk_tp t ecall.epos ) params) @ elist) }
  );

  HardNullableSynf.configure gen (HardNullableSynf.traverse gen
    (fun e ->
      match real_type e.etype with
        | TInst({ cl_path = (["haxe";"lang"], "Null") }, [t]) ->
            { (mk_field_access gen e "value" e.epos) with etype = t }
        | _ ->
          trace (debug_type e.etype); gen.gcon.error "This expression is not a Nullable expression" e.epos; assert false
    )
    (fun v t has_value ->
      match has_value, real_type v.etype with
        | true, TDynamic _ | true, TAnon _ | true, TMono _ ->
          {
            eexpr = TCall(mk_static_field_access_infer null_t "ofDynamic" v.epos [t], [mk_tp t v.epos; v]);
            etype = TInst(null_t, [t]);
            epos = v.epos
          }
        | _ ->
          { eexpr = TNew(null_t, [t], [gen.ghandle_cast t v.etype v; { eexpr = TConst(TBool has_value); etype = gen.gcon.basic.tbool; epos = v.epos } ]); etype = TInst(null_t, [t]); epos = v.epos }
    )
    (fun e ->
      {
        eexpr = TCall(
          { (mk_field_access gen { (mk_paren e) with etype = real_type e.etype } "toDynamic" e.epos) with etype = TFun([], t_dynamic) },
          []);
        etype = t_dynamic;
        epos = e.epos
      }
    )
    (fun e ->
      mk_field_access gen { e with etype = real_type e.etype } "hasValue" e.epos
    )
    (fun e1 e2 ->
      {
        eexpr = TCall(
          mk_field_access gen e1 "Equals" e1.epos,
          [e2]);
        etype = basic.tbool;
        epos = e1.epos;
      }
    )
    true
    false
  );


  let explicit_fn_name c tl fname =
    path_param_s (TClassDecl c) c.cl_path tl ^ "." ^ fname
  in
  FixOverrides.configure ~explicit_fn_name:explicit_fn_name gen;
  Normalize.configure gen ~metas:(Hashtbl.create 0);

  AbstractImplementationFix.configure gen;

  IteratorsInterface.configure gen (fun e -> e);

  OverrideFix.configure gen;

  ClosuresToClass.configure gen (ClosuresToClass.default_implementation closure_t (get_cl (get_type gen (["haxe";"lang"],"Function")) ));

  EnumToClass.configure gen (Some (fun e -> mk_cast gen.gcon.basic.tint e)) false true (get_cl (get_type gen (["haxe";"lang"],"Enum")) ) true false;

  InterfaceVarsDeleteModf.configure gen;

  let dynamic_object = (get_cl (get_type gen (["haxe";"lang"],"DynamicObject")) ) in

  let object_iface = get_cl (get_type gen (["haxe";"lang"],"IHxObject")) in

  (*fixme: THIS IS A HACK. take this off *)
  let empty_e = match (get_type gen (["haxe";"lang"], "EmptyObject")) with | TEnumDecl e -> e | _ -> assert false in
  (*OverloadingCtor.set_new_create_empty gen ({eexpr=TEnumField(empty_e, "EMPTY"); etype=TEnum(empty_e,[]); epos=null_pos;});*)

  let empty_expr = { eexpr = (TTypeExpr (TEnumDecl empty_e)); etype = (TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics empty_e) }); epos = null_pos } in
  let empty_ef =
    try
      PMap.find "EMPTY" empty_e.e_constrs
    with Not_found -> gen.gcon.error "Required enum field EMPTY was not found" empty_e.e_pos; assert false
  in
  OverloadingConstructor.configure ~empty_ctor_type:(TEnum(empty_e, [])) ~empty_ctor_expr:({ eexpr=TField(empty_expr, FEnum(empty_e, empty_ef)); etype=TEnum(empty_e,[]); epos=null_pos; }) ~supports_ctor_inheritance:false gen;

  let rcf_static_find = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "findHash" Ast.null_pos [] in
  let rcf_static_lookup = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "lookupHash" Ast.null_pos [] in

  let can_be_float = like_float in

  let rcf_on_getset_field main_expr field_expr field may_hash may_set is_unsafe =
    let is_float = can_be_float (real_type main_expr.etype) in
    let fn_name = if is_some may_set then "setField" else "getField" in
    let fn_name = if is_float then fn_name ^ "_f" else fn_name in
    let pos = field_expr.epos in

    let is_unsafe = { eexpr = TConst(TBool is_unsafe); etype = basic.tbool; epos = pos } in

    let should_cast = match main_expr.etype with | TInst({ cl_path = ([], "Float") }, []) -> false | _ -> true in
    let infer = mk_static_field_access_infer runtime_cl fn_name field_expr.epos [] in
    let first_args =
      [ field_expr; { eexpr = TConst(TString field); etype = basic.tstring; epos = pos } ]
      @ if is_some may_hash then [ { eexpr = TConst(TInt (get may_hash)); etype = basic.tint; epos = pos } ] else []
    in
    let args = first_args @ match is_float, may_set with
      | true, Some(set) ->
        [ if should_cast then mk_cast basic.tfloat set else set ]
      | false, Some(set) ->
        [ set ]
      | _ ->
        [ is_unsafe ]
    in

    let call = { main_expr with eexpr = TCall(infer,args) } in
    let call = if is_float && should_cast then mk_cast main_expr.etype call else call in
    call
  in

  let rcf_on_call_field ecall field_expr field may_hash args =
    let infer = mk_static_field_access_infer runtime_cl "callField" field_expr.epos [] in

    let hash_arg = match may_hash with
      | None -> []
      | Some h -> [ { eexpr = TConst(TInt h); etype = basic.tint; epos = field_expr.epos } ]
    in

    let arr_call = if args <> [] then
      { eexpr = TArrayDecl args; etype = basic.tarray t_dynamic; epos = ecall.epos }
    else
      null (basic.tarray t_dynamic) ecall.epos
    in

    let call_args =
      [field_expr; { field_expr with eexpr = TConst(TString field); etype = basic.tstring } ]
        @ hash_arg
        @ [ arr_call ]
    in

    mk_cast ecall.etype { ecall with eexpr = TCall(infer, call_args) }
  in

  handle_type_params gen ifaces (get_cl (get_type gen (["haxe";"lang"], "IGenericObject")));

  let rcf_ctx = ReflectionCFs.new_ctx gen closure_t object_iface true rcf_on_getset_field rcf_on_call_field (fun hash hash_array ->
    { hash with eexpr = TCall(rcf_static_find, [hash; hash_array]); etype=basic.tint }
  ) (fun hash -> { hash with eexpr = TCall(rcf_static_lookup, [hash]); etype = gen.gcon.basic.tstring } ) false in

  ReflectionCFs.UniversalBaseClass.default_config gen (get_cl (get_type gen (["haxe";"lang"],"HxObject")) ) object_iface dynamic_object;

  ReflectionCFs.configure_dynamic_field_access rcf_ctx false;

  (* let closure_func = ReflectionCFs.implement_closure_cl rcf_ctx ( get_cl (get_type gen (["haxe";"lang"],"Closure")) ) in *)
  let closure_cl = get_cl (get_type gen (["haxe";"lang"],"Closure")) in
  let varargs_cl = get_cl (get_type gen (["haxe";"lang"],"VarArgsFunction")) in
  let dynamic_name = gen.gmk_internal_name "hx" "invokeDynamic" in

  List.iter (fun cl ->
    List.iter (fun cf ->
      if cf.cf_name = dynamic_name then cl.cl_overrides <- cf :: cl.cl_overrides
    ) cl.cl_ordered_fields
  ) [closure_cl; varargs_cl];

  let closure_func = ReflectionCFs.get_closure_func rcf_ctx closure_cl in

  ReflectionCFs.implement_varargs_cl rcf_ctx ( get_cl (get_type gen (["haxe";"lang"], "VarArgsBase")) );

  let slow_invoke = mk_static_field_access_infer (runtime_cl) "slowCallField" Ast.null_pos [] in
  ReflectionCFs.configure rcf_ctx ~slow_invoke:(fun ethis efield eargs -> {
    eexpr = TCall(slow_invoke, [ethis; efield; eargs]);
    etype = t_dynamic;
    epos = ethis.epos;
  } ) object_iface;

  let objdecl_fn = ReflectionCFs.implement_dynamic_object_ctor rcf_ctx dynamic_object in

  ObjectDeclMap.configure gen (ObjectDeclMap.traverse gen objdecl_fn);

  InitFunction.configure gen true;
  TArrayTransform.configure gen (TArrayTransform.default_implementation gen (
  fun e ->
    match e.eexpr with
      | TArray(e1, e2) ->
        ( match follow e1.etype with
          | TDynamic _ | TAnon _ | TMono _ -> true
          | TInst({ cl_kind = KTypeParameter _ }, _) -> true
          | _ -> false )
      | _ -> assert false
  ) "__get" "__set" );

  let field_is_dynamic t field =
    match field_access gen (gen.greal_type t) field with
      | FEnumField _
      | FClassField _ -> false
      | _ -> true
  in

  let is_type_param e = match follow e with
    | TInst( { cl_kind = KTypeParameter _ },[]) -> true
    | _ -> false
  in

  let is_dynamic_expr e = is_dynamic e.etype || match e.eexpr with
    | TField(tf, f) -> field_is_dynamic tf.etype (field_name f)
    | _ -> false
  in

  let may_nullable t = match gen.gfollow#run_f t with
    | TType({ t_path = ([], "Null") }, [t]) ->
      (match follow t with
        | TInst({ cl_path = ([], "String") }, [])
        | TInst({ cl_path = ([], "Float") }, [])
        | TAbstract ({ a_path = ([], "Float") },[])
        | TInst({ cl_path = (["haxe"], "Int32")}, [] )
        | TInst({ cl_path = (["haxe"], "Int64")}, [] )
        | TInst({ cl_path = ([], "Int") }, [])
        | TAbstract ({ a_path = ([], "Int") },[])
        | TEnum({ e_path = ([], "Bool") }, [])
        | TAbstract ({ a_path = ([], "Bool") },[]) -> Some t
        | TAbstract _ when like_float t -> Some t
        | _ -> None )
    | _ -> None
  in

  let is_double t = like_float t && not (like_int t) in
  let is_int t = like_int t in

  let is_null t = match real_type t with
    | TInst( { cl_path = (["haxe";"lang"], "Null") }, _ ) -> true
    | _ -> false
  in

  let is_null_expr e = is_null e.etype || match e.eexpr with
    | TField(tf, f) -> (match field_access gen (real_type tf.etype) (field_name f) with
      | FClassField(_,_,_,_,_,actual_t,_) -> is_null actual_t
      | _ -> false)
    | _ -> false
  in

  let should_handle_opeq t =
    match real_type t with
      | TDynamic _ | TAnon _ | TMono _
      | TInst( { cl_kind = KTypeParameter _ }, _ )
      | TInst( { cl_path = (["haxe";"lang"], "Null") }, _ ) -> true
      | _ -> false
  in

  let string_cl = match gen.gcon.basic.tstring with
    | TInst(c,[]) -> c
    | _ -> assert false
  in

  DynamicOperators.configure gen
    (DynamicOperators.abstract_implementation gen (fun e -> match e.eexpr with
      | TBinop (Ast.OpEq, e1, e2)
      | TBinop (Ast.OpNotEq, e1, e2) -> should_handle_opeq e1.etype or should_handle_opeq e2.etype
      | TBinop (Ast.OpAssignOp Ast.OpAdd, e1, e2) ->
        is_dynamic_expr e1 || is_null_expr e1 || is_string e.etype
      | TBinop (Ast.OpAdd, e1, e2) -> is_dynamic e1.etype or is_dynamic e2.etype or is_type_param e1.etype or is_type_param e2.etype or is_string e1.etype or is_string e2.etype or is_string e.etype
      | TBinop (Ast.OpLt, e1, e2)
      | TBinop (Ast.OpLte, e1, e2)
      | TBinop (Ast.OpGte, e1, e2)
      | TBinop (Ast.OpGt, e1, e2) -> is_dynamic e.etype or is_dynamic_expr e1 or is_dynamic_expr e2 or is_string e1.etype or is_string e2.etype
      | TBinop (_, e1, e2) -> is_dynamic e.etype or is_dynamic_expr e1 or is_dynamic_expr e2
      | TUnop (_, _, e1) -> is_dynamic_expr e1 || is_null_expr e1 (* we will see if the expression is Null<T> also, as the unwrap from Unop will be the same *)
      | _ -> false)
    (fun e1 e2 ->
      let is_basic = is_cs_basic_type (follow e1.etype) || is_cs_basic_type (follow e2.etype) in
      let is_ref = if is_basic then false else match follow e1.etype, follow e2.etype with
        | TDynamic _, _
        | _, TDynamic _
        | TInst( { cl_path = ([], "String") }, [] ), _
        | _, TInst( { cl_path = ([], "String") }, [] )
        | TInst( { cl_kind = KTypeParameter _ }, [] ), _
        | _, TInst( { cl_kind = KTypeParameter _ }, [] ) -> false
        | _, _ -> true
      in

      let static = mk_static_field_access_infer (runtime_cl) (if is_ref then "refEq" else "eq") e1.epos [] in
      { eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tbool; epos=e1.epos }
    )
    (fun e e1 e2 ->
      match may_nullable e1.etype, may_nullable e2.etype with
        | Some t1, Some t2 ->
          let t1, t2 = if is_string t1 || is_string t2 then
            basic.tstring, basic.tstring
          else if is_double t1 || is_double t2 then
            basic.tfloat, basic.tfloat
          else if is_int t1 || is_int t2 then
            basic.tint, basic.tint
          else t1, t2 in
          { eexpr = TBinop(Ast.OpAdd, mk_cast t1 e1, mk_cast t2 e2); etype = e.etype; epos = e1.epos }
        | _ when is_string e.etype || is_string e1.etype || is_string e2.etype ->
          {
            eexpr = TCall(
              mk_static_field_access_infer runtime_cl "concat" e.epos [],
              [ e1; e2 ]
            );
            etype = basic.tstring;
            epos = e.epos
          }
        | _ ->
          let static = mk_static_field_access_infer (runtime_cl) "plus"  e1.epos [] in
          mk_cast e.etype { eexpr = TCall(static, [e1; e2]); etype = t_dynamic; epos=e1.epos })
    (fun e1 e2 ->
      if is_string e1.etype then begin
        { e1 with eexpr = TCall(mk_static_field_access_infer string_cl "Compare" e1.epos [], [ e1; e2 ]); etype = gen.gcon.basic.tint }
      end else begin
        let static = mk_static_field_access_infer (runtime_cl) "compare" e1.epos [] in
        { eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tint; epos=e1.epos }
      end) ~handle_strings:false);

  FilterClosures.configure gen (FilterClosures.traverse gen (fun e1 s -> true) closure_func);

  let base_exception = get_cl (get_type gen (["System"], "Exception")) in
  let base_exception_t = TInst(base_exception, []) in

  let hx_exception = get_cl (get_type gen (["haxe";"lang"], "HaxeException")) in
  let hx_exception_t = TInst(hx_exception, []) in

  let rec is_exception t =
    match follow t with
      | TInst(cl,_) ->
        if cl == base_exception then
          true
        else
          (match cl.cl_super with | None -> false | Some (cl,arg) -> is_exception (TInst(cl,arg)))
      | _ -> false
  in

  TryCatchWrapper.configure gen
  (
    TryCatchWrapper.traverse gen
      (fun t -> not (is_exception (real_type t)))
      (fun throwexpr expr ->
        let wrap_static = mk_static_field_access (hx_exception) "wrap" (TFun([("obj",false,t_dynamic)], base_exception_t)) expr.epos in
        { throwexpr with eexpr = TThrow { expr with eexpr = TCall(wrap_static, [expr]); etype = hx_exception_t }; etype = gen.gcon.basic.tvoid }
      )
      (fun v_to_unwrap pos ->
        let local = mk_cast hx_exception_t { eexpr = TLocal(v_to_unwrap); etype = v_to_unwrap.v_type; epos = pos } in
        mk_field_access gen local "obj" pos
      )
      (fun rethrow ->
        { rethrow with eexpr = TCall(mk_local (alloc_var "__rethrow__" t_dynamic) rethrow.epos, [rethrow]); etype = gen.gcon.basic.tvoid }
      )
      (base_exception_t)
      (hx_exception_t)
      (fun v e -> e)
  );

  let get_typeof e =
    { e with eexpr = TCall( { eexpr = TLocal( alloc_var "__typeof__" t_dynamic ); etype = t_dynamic; epos = e.epos }, [e] ) }
  in

  ClassInstance.configure gen (ClassInstance.traverse gen (fun e mt ->
    get_typeof e
  ));

  CastDetect.configure gen (CastDetect.default_implementation gen (Some (TEnum(empty_e, []))) true ~native_string_cast:false ~overloads_cast_to_base:true);

  (*FollowAll.configure gen;*)

  SwitchToIf.configure gen (SwitchToIf.traverse gen (fun e ->
    match e.eexpr with
      | TSwitch(cond, cases, def) ->
        (match gen.gfollow#run_f cond.etype with
          | TInst({ cl_path = ([], "Int") },[])
          | TAbstract ({ a_path = ([], "Int") },[])
          | TInst({ cl_path = ([], "String") },[]) ->
            (List.exists (fun (c,_) ->
              List.exists (fun expr -> match expr.eexpr with | TConst _ -> false | _ -> true ) c
            ) cases)
          | _ -> true
        )
      | _ -> assert false
  ) true ) ;

  ExpressionUnwrap.configure gen (ExpressionUnwrap.traverse gen (fun e -> Some { eexpr = TVars([mk_temp gen "expr" e.etype, Some e]); etype = gen.gcon.basic.tvoid; epos = e.epos }));

  UnnecessaryCastsRemoval.configure gen;

  IntDivisionSynf.configure gen (IntDivisionSynf.default_implementation gen true);

  UnreachableCodeEliminationSynf.configure gen (UnreachableCodeEliminationSynf.traverse gen false true true false);

  let native_arr_cl = get_cl ( get_type gen (["cs"], "NativeArray") ) in
  ArrayDeclSynf.configure gen (ArrayDeclSynf.default_implementation gen native_arr_cl);

  let goto_special = alloc_var "__goto__" t_dynamic in
  let label_special = alloc_var "__label__" t_dynamic in
  SwitchBreakSynf.configure gen (SwitchBreakSynf.traverse gen
    (fun e_loop n api ->
      api ({ eexpr = TCall( mk_local label_special e_loop.epos, [ mk_int gen n e_loop.epos ] ); etype = t_dynamic; epos = e_loop.epos }) false;
      e_loop
    )
    (fun e_break n api ->
      { eexpr = TCall( mk_local goto_special e_break.epos, [ mk_int gen n e_break.epos ] ); etype = t_dynamic; epos = e_break.epos }
    )
  );

  DefaultArguments.configure gen (DefaultArguments.traverse gen);

  CSharpSpecificSynf.configure gen (CSharpSpecificSynf.traverse gen runtime_cl);
  CSharpSpecificESynf.configure gen (CSharpSpecificESynf.traverse gen runtime_cl);

  let mkdir dir = if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 in
  mkdir gen.gcon.file;
  mkdir (gen.gcon.file ^ "/src");

  (* add resources array *)
  (try
    let res = get_cl (Hashtbl.find gen.gtypes (["haxe"], "Resource")) in
    mkdir (gen.gcon.file ^ "/src/Resources");
    let cf = PMap.find "content" res.cl_statics in
    let res = ref [] in
    Hashtbl.iter (fun name v ->
      res := { eexpr = TConst(TString name); etype = gen.gcon.basic.tstring; epos = Ast.null_pos } :: !res;

      let f = open_out (gen.gcon.file ^ "/src/Resources/" ^ name) in
      output_string f v;
      close_out f
    ) gen.gcon.resources;
    cf.cf_expr <- Some ({ eexpr = TArrayDecl(!res); etype = gen.gcon.basic.tarray gen.gcon.basic.tstring; epos = Ast.null_pos })
  with | Not_found -> ());

  run_filters gen;
  (* after the filters have been run, add all hashed fields to FieldLookup *)

  let normalize_i i =
    let i = Int32.of_int (i) in
    if i < Int32.zero then
      Int32.logor (Int32.logand i (Int32.of_int 0x3FFFFFFF)) (Int32.shift_left Int32.one 30)
    else i
  in

  let hashes = Hashtbl.fold (fun i s acc -> (normalize_i i,s) :: acc) rcf_ctx.rcf_hash_fields [] in
  let hashes = List.sort (fun (i,s) (i2,s2) -> compare i i2) hashes in

  let flookup_cl = get_cl (get_type gen (["haxe";"lang"], "FieldLookup")) in
  (try
    let basic = gen.gcon.basic in
    let change_array = ArrayDeclSynf.default_implementation gen native_arr_cl in
    let cl = flookup_cl in
    let field_ids = PMap.find "fieldIds" cl.cl_statics in
    let fields = PMap.find "fields" cl.cl_statics in

    field_ids.cf_expr <- Some (change_array {
      eexpr = TArrayDecl(List.map (fun (i,s) -> { eexpr = TConst(TInt (i)); etype = basic.tint; epos = field_ids.cf_pos }) hashes);
      etype = basic.tarray basic.tint;
      epos = field_ids.cf_pos
    });

    fields.cf_expr <- Some (change_array {
      eexpr = TArrayDecl(List.map (fun (i,s) -> { eexpr = TConst(TString s); etype = basic.tstring; epos = fields.cf_pos }) hashes);
      etype = basic.tarray basic.tstring;
      epos = fields.cf_pos
    })

  with | Not_found ->
    gen.gcon.error "Fields 'fieldIds' and 'fields' were not found in class haxe.lang.FieldLookup" flookup_cl.cl_pos
  );

  TypeParams.RenameTypeParameters.run gen;

  let t = Common.timer "code generation" in

	generate_modules gen "cs" "src" module_gen;

  dump_descriptor gen ("hxcs_build.txt") path_s module_s;
	if ( not (Common.defined gen.gcon Define.NoCompilation) ) then begin
		let old_dir = Sys.getcwd() in
		Sys.chdir gen.gcon.file;
		let cmd = "haxelib run hxcs hxcs_build.txt --haxe-version " ^ (string_of_int gen.gcon.version) in
		print_endline cmd;
		if gen.gcon.run_command cmd <> 0 then failwith "Build failed";
		Sys.chdir old_dir;
	end;

  t()

(* end of configure function *)

let generate con =
  (try
    let gen = new_ctx con in
    let basic = con.basic in

    (* make the basic functions in C# *)
    let type_cl = get_cl ( get_type gen (["System"], "Type")) in
    let basic_fns =
    [
      mk_class_field "Equals" (TFun(["obj",false,t_dynamic], basic.tbool)) true Ast.null_pos (Method MethNormal) [];
      mk_class_field "ToString" (TFun([], basic.tstring)) true Ast.null_pos (Method MethNormal) [];
      mk_class_field "GetHashCode" (TFun([], basic.tint)) true Ast.null_pos (Method MethNormal) [];
      mk_class_field "GetType" (TFun([], TInst(type_cl, []))) true Ast.null_pos (Method MethNormal) [];
    ] in
    List.iter (fun cf -> gen.gbase_class_fields <- PMap.add cf.cf_name cf gen.gbase_class_fields) basic_fns;
    configure gen
  with | TypeNotFound path ->
    con.error ("Error. Module '" ^ (path_s path) ^ "' is required and was not included in build.")  Ast.null_pos);
  debug_mode := false

(* -net-lib implementation *)
open IlData
open IlMeta

type net_lib_ctx = {
	nstd : bool;
	ncom : Common.context;
	nil : IlData.ilctx;
}

let netname_to_hx name =
	let len = String.length name in
	let chr = String.get name 0 in
	String.make 1 (Char.uppercase chr) ^ (String.sub name 1 (len-1))

let hxpath_to_net ctx path =
	try
		Hashtbl.find ctx.ncom.net_path_map path
	with
	 | Not_found ->
			[],[],"Not_found"

let add_cs = function
	| "haxe" :: ns -> "haxe" :: ns
	| "std" :: ns -> "std" :: ns
	| "cs" :: ns -> "cs" :: ns
	| ns -> "cs" :: ns

let netpath_to_hx std = function
	| [],[], cl -> [], cl
	| ns,[], cl ->
		let ns = (List.map String.lowercase ns) in
		(if std then add_cs ns else ns), cl
	| ns,(nhd :: ntl as nested), cl ->
		let ns = (List.map String.lowercase ns) @ [nhd] in
		(if std then add_cs ns else ns), String.concat "_" nested ^ "_" ^ cl

let lookup_ilclass std com ilpath =
  let path = netpath_to_hx std ilpath in
  List.fold_right (fun (_,_,_,get_raw_class) acc ->
    match acc with
    | None -> get_raw_class path
    | Some p -> acc
  ) com.net_libs None

let discard_nested = function
	| (ns,_),cl -> (ns,[]),cl

let mk_type_path ctx path params =
  let pack, sub, name = match path with
		| ns,[], cl ->
			ns, None, cl
		| ns, (nhd :: ntl as nested), cl ->
			ns, Some (String.concat "_" nested ^ "_" ^ cl), nhd
	in
  CTPath {
		tpackage = fst (netpath_to_hx ctx.nstd (pack,[],""));
    Ast.tname = name;
    tparams = params;
    tsub = sub;
  }

let raw_type_path ctx path params =
	{
		tpackage = fst path;
		Ast.tname = snd path;
		tparams = params;
		tsub = None;
	}

let rec convert_signature ctx p = function
	| LVoid ->
		mk_type_path ctx ([],[],"Void") []
	| LBool ->
		mk_type_path ctx ([],[],"Bool") []
	| LChar ->
		mk_type_path ctx (["cs";"types"],[],"Char16") []
	| LInt8 ->
		mk_type_path ctx (["cs";"types"],[],"Int8") []
	| LUInt8 ->
		mk_type_path ctx (["cs";"types"],[],"UInt8") []
	| LInt16 ->
		mk_type_path ctx (["cs";"types"],[],"Int16") []
	| LUInt16 ->
		mk_type_path ctx (["cs";"types"],[],"UInt16") []
	| LInt32 ->
		mk_type_path ctx ([],[],"Int") []
	| LUInt32 ->
		mk_type_path ctx ([],[],"UInt") []
	| LInt64 ->
		mk_type_path ctx (["haxe"],[],"Int64") []
	| LUInt64 ->
		mk_type_path ctx (["cs";"types"],[],"UInt64") []
	| LFloat32 ->
		mk_type_path ctx ([],[],"Single") []
	| LFloat64 ->
		mk_type_path ctx ([],[],"Float") []
	| LString ->
		mk_type_path ctx (["std"],[],"String") []
	| LObject ->
		mk_type_path ctx ([],[],"Dynamic") []
	| LPointer s | LManagedPointer s ->
		mk_type_path ctx (["cs"],[],"Pointer") [ TPType (convert_signature ctx p s) ]
	| LTypedReference ->
		mk_type_path ctx (["cs";"system"],[],"TypedReference") []
	| LIntPtr ->
		mk_type_path ctx (["cs";"system"],[],"IntPtr") []
	| LUIntPtr ->
		mk_type_path ctx (["cs";"system"],[],"UIntPtr") []
	| LValueType (s,args) | LClass (s,args) ->
		mk_type_path ctx s (List.map (fun s -> TPType (convert_signature ctx p s)) args)
	| LTypeParam i ->
		mk_type_path ctx ([],[],"T" ^ string_of_int i) []
	| LMethodTypeParam i ->
		mk_type_path ctx ([],[],"M" ^ string_of_int i) []
	| LVector s ->
		mk_type_path ctx (["cs"],[],"NativeArray") [TPType (convert_signature ctx p s)]
	(* | LArray of ilsig_norm * (int option * int option) array *)
	| LMethod (_,ret,args) ->
		CTFunction (List.map (convert_signature ctx p) args, convert_signature ctx p ret)
	| _ -> mk_type_path ctx ([],[], "Dynamic") []

let ilpath_s = function
	| ns,[], name -> path_s (ns,name)
	| [],nested,name -> String.concat "#" nested ^ "." ^ name
	| ns, nested, name -> String.concat "." ns ^ "." ^ String.concat "#" nested ^ "." ^ name

let get_cls = function
	| _,_,c -> c

let convert_ilenum ctx p ilcls =
  let meta = ref [Meta.Native, [EConst (String (ilpath_s ilcls.cpath) ), p], p ] in
  let data = ref [] in
  List.iter (fun f -> match f.fname with
		| "value__" -> ()
		| _ ->
      data := { ec_name = f.fname; ec_doc = None; ec_meta = []; ec_args = []; ec_pos = p; ec_params = []; ec_type = None; } :: !data;
  ) ilcls.cfields;
	let _, c = netpath_to_hx ctx.nstd ilcls.cpath in
  EEnum {
		d_name = netname_to_hx c;
		d_doc = None;
		d_params = []; (* enums never have type parameters *)
		d_meta = !meta;
		d_flags = [EExtern];
		d_data = !data;
  }

let rec has_unmanaged = function
	| LPointer _ -> true
	| LManagedPointer s -> has_unmanaged s
	| LValueType (p,pl) -> List.exists (has_unmanaged) pl
	| LClass (p,pl) -> List.exists (has_unmanaged) pl
	| LVector s -> has_unmanaged s
	| LArray (s,a) -> has_unmanaged s
	| LMethod (c,r,args) -> has_unmanaged r || List.exists (has_unmanaged) args
	| _ -> false

let convert_ilfield ctx p field =
	if not (Common.defined ctx.ncom Define.Unsafe) && has_unmanaged field.fsig.snorm then raise Exit;
	let p = { p with pfile =  p.pfile ^" (" ^field.fname ^")" } in
	let cff_doc = None in
	let cff_pos = p in
	let cff_meta = ref [] in
	let cff_name = match field.fname with
		| name when String.length name > 5 ->
				(match String.sub name 0 5 with
				| "__hx_" -> raise Exit
				| _ -> name)
		| name -> name
	in
	let cff_access = match field.fflags.ff_access with
		| FAFamily | FAFamOrAssem -> APrivate
		| FAPublic -> APublic
		| _ -> raise Exit (* private instances aren't useful on externs *)
	in
	let readonly, acc = List.fold_left (fun (readonly,acc) -> function
		| CStatic -> readonly, AStatic :: acc
		| CInitOnly | CLiteral -> true, acc
		| _ -> readonly,acc
	) (false,[cff_access]) field.fflags.ff_contract in
	let kind = match readonly with
		| true ->
			FProp ("default", "never", Some (convert_signature ctx p field.fsig.snorm), None)
		| false ->
			FVar (Some (convert_signature ctx p field.fsig.snorm), None)
	in
	let cff_name, cff_meta =
		if String.get cff_name 0 = '%' then
			let name = (String.sub cff_name 1 (String.length cff_name - 1)) in
			"_" ^ name,
			(Meta.Native, [EConst (String (name) ), cff_pos], cff_pos) :: !cff_meta
		else
			cff_name, !cff_meta
	in
	{
		cff_name = cff_name;
		cff_doc = cff_doc;
		cff_pos = cff_pos;
		cff_meta = cff_meta;
		cff_access = acc;
		cff_kind = kind;
	}

let convert_ilmethod ctx p m =
	if not (Common.defined ctx.ncom Define.Unsafe) && has_unmanaged m.msig.snorm then raise Exit;
	let p = { p with pfile =  p.pfile ^" (" ^m.mname ^")" } in
	let cff_doc = None in
	let cff_pos = p in
	let cff_name = match m.mname with
		| ".ctor" -> "new"
		| ".cctor"-> raise Exit (* __init__ field *)
		| "Equals" | "GetHashCode" -> raise Exit
		| name when String.length name > 5 ->
				(match String.sub name 0 5 with
				| "__hx_" -> raise Exit
				| _ -> name)
		| name -> name
	in
	let acc = match m.mflags.mf_access with
		| FAFamily | FAFamOrAssem -> APrivate
		(* | FAPrivate -> APrivate *)
		| FAPublic -> APublic
		| _ -> raise Exit
	in
	if PMap.mem "net_loader_debug" ctx.ncom.defines then
		Printf.printf "\tname %s : %s\n" cff_name (IlMetaDebug.ilsig_s m.msig.ssig);
	let is_static = ref false in
	let acc, is_final = List.fold_left (fun (acc,is_final) -> function
		| CMStatic when cff_name <> "new" -> is_static := true; AStatic :: acc, is_final
		| CMVirtual when is_final = None -> acc, Some false
		| CMFinal -> acc, Some true
		| _ -> acc, is_final
	) ([acc],None) m.mflags.mf_contract in

	let meta = [Meta.Overload, [], p] in
	let meta = match is_final with
		| None | Some false ->
			(Meta.Final, [], p) :: meta
		| _ -> meta
	in
	(* let meta = if List.mem OSynchronized m.mflags.mf_interop then *)
	(* 	(Meta.Synchronized,[],p) :: meta *)
	(* else *)
	(* 	meta *)
	(* in *)

	let rec change_sig = function
		| LManagedPointer s -> LManagedPointer (change_sig s)
		| LPointer s -> LPointer (change_sig s)
		| LValueType (p,pl) -> LValueType(p, List.map change_sig pl)
		| LClass (p,pl) -> LClass(p, List.map change_sig pl)
		| LTypeParam i -> LObject
		| LVector s -> LVector (change_sig s)
		| LArray (s,a) -> LArray (change_sig s, a)
		| LMethod (c,r,args) -> LMethod (c, change_sig r, List.map change_sig args)
		| p -> p
	in
	let change_sig = if !is_static then change_sig else (fun s -> s) in

	let ret =
		if String.length cff_name > 4 && String.sub cff_name 0 4 = "set_" then
			match m.mret.snorm, m.margs with
			| LVoid, [_,_,s] ->
				s.snorm
			| _ -> m.mret.snorm
		else
			m.mret.snorm
	in

	let kind =
		let args = List.map (fun (name,flag,s) ->
			let t = match s.snorm with
				| LManagedPointer s ->
					mk_type_path ctx (["cs"],[],"Ref") [ TPType (convert_signature ctx p s) ]
				| _ ->
					convert_signature ctx p (change_sig s.snorm)
			in
			name,false,Some t,None) m.margs
		in
		let ret = convert_signature ctx p (change_sig ret) in
		let types = List.map (fun t ->
			{
				tp_name = "M" ^ string_of_int t.tnumber;
				tp_params = [];
				tp_constraints = [];
			}
		) m.mtypes in
		FFun {
			f_params = types;
			f_args = args;
			f_type = Some ret;
			f_expr = None;
		}
	in
	let cff_name, cff_meta =
		if String.get cff_name 0 = '%' then
			let name = (String.sub cff_name 1 (String.length cff_name - 1)) in
			"_" ^ name,
			(Meta.Native, [EConst (String (name) ), cff_pos], cff_pos) :: meta
		else
			cff_name, meta
	in
	let acc = match m.moverride with
		| None -> acc
		| Some (path,s) -> match lookup_ilclass ctx.nstd ctx.ncom path with
			| Some ilcls when not (List.mem SInterface ilcls.cflags.tdf_semantics) ->
				AOverride :: acc
			| None when ctx.ncom.verbose ->
				prerr_endline ("(net-lib) A referenced assembly for path " ^ ilpath_s path ^ " was not found");
				acc
			| _ -> acc
	in
	{
		cff_name = cff_name;
		cff_doc = cff_doc;
		cff_pos = cff_pos;
		cff_meta = cff_meta;
		cff_access = acc;
		cff_kind = kind;
	}

let convert_ilprop ctx p prop =
	if not (Common.defined ctx.ncom Define.Unsafe) && has_unmanaged prop.psig.snorm then raise Exit;
	let p = { p with pfile =  p.pfile ^" (" ^prop.pname ^")" } in
	let cff_access = match prop.pmflags with
		| Some { mf_access = FAFamily | FAFamOrAssem } -> APrivate
		| Some { mf_access = FAPublic } -> APublic
		| _ -> raise Exit (* private instances aren't useful on externs *)
	in
	let cff_access = match prop.pmflags with
		| Some m when List.mem CMStatic m.mf_contract ->
			[AStatic;cff_access]
		| _ -> [cff_access]
	in
	let get = match prop.pget with
		| None -> "never"
		| Some s when String.length s <= 4 || String.sub s 0 4 <> "get_" ->
			raise Exit (* special (?) getter; not used *)
		| Some _ -> "get"
	in
	let set = match prop.pset with
		| None -> "never"
		| Some s when String.length s <= 4 || String.sub s 0 4 <> "set_" ->
			raise Exit (* special (?) getter; not used *)
		| Some _ -> "set"
	in
	if PMap.mem "net_loader_debug" ctx.ncom.defines then
		Printf.printf "\tproperty %s (%s,%s) : %s\n" prop.pname get set (IlMetaDebug.ilsig_s prop.psig.ssig);
	let ilsig = match prop.psig.snorm with
		| LMethod (_,ret,[]) -> ret
		| s -> raise Exit
	in

	let kind =
		FProp (get, set, Some(convert_signature ctx p ilsig), None)
	in
	{
		cff_name = prop.pname;
		cff_doc = None;
		cff_pos = p;
		cff_meta = [];
		cff_access = cff_access;
		cff_kind = kind;
	}

let get_type_path ctx ct = match ct with | CTPath p -> p | _ -> assert false

let is_explicit ctx ilcls i =
	let s = match i with
		| LClass(path,_) | LValueType(path,_) -> ilpath_s path
		| _ -> assert false
	in
	let len = String.length s in
	List.exists (fun m ->
		String.length m.mname > len && String.sub m.mname 0 len = s
	) ilcls.cmethods

let convert_ilclass ctx p ilcls = match ilcls.csuper with
	| Some { snorm = LClass ((["System"],[],"Enum"),[]) } ->
		convert_ilenum ctx p ilcls
	| _ ->
		let flags = ref [HExtern] in
		(* todo: instead of CsNative, use more specific definitions *)
		if PMap.mem "net_loader_debug" ctx.ncom.defines then
			print_endline ("converting " ^ ilpath_s ilcls.cpath);
		let meta = ref [Meta.CsNative, [], p; Meta.Native, [EConst (String (ilpath_s ilcls.cpath) ), p], p] in

		let is_interface = ref false in
		List.iter (fun f -> match f with
			| SSealed -> meta := (Meta.Final, [], p) :: !meta
			| SInterface ->
				is_interface := true;
				flags := HInterface :: !flags
			| SAbstract -> meta := (Meta.Abstract, [], p) :: !meta
			| _ -> ()
		) ilcls.cflags.tdf_semantics;

		(* (match ilcls.cflags.tdf_vis with *)
		(* 	| VPublic | VNestedFamOrAssem | VNestedFamily -> () *)
		(* 	| _ -> raise Exit); *)
		(match ilcls.csuper with
			| Some { snorm = LClass ( (["System"],[],"Object"), [] ) } -> ()
			| Some { snorm = LClass ( (["haxe";"lang"],[],"HxObject"), [] ) } ->
				meta := (Meta.HxGen,[],p) :: !meta
			| Some s ->
				flags := HExtends (get_type_path ctx (convert_signature ctx p s.snorm)) :: !flags
			| _ -> ());

			List.iter (fun i ->
				match i.snorm with
				| LClass ( (["haxe";"lang"],[], "IHxObject"), _ ) ->
					meta := (Meta.HxGen,[],p) :: !meta
				| i when is_explicit ctx ilcls i -> ()
				| i -> flags :=
					if !is_interface then
						HExtends (get_type_path ctx (convert_signature ctx p i)) :: !flags
					else
						HImplements (get_type_path ctx (convert_signature ctx p i)) :: !flags
			) ilcls.cimplements;

			(* ArrayAccess *)
			ignore (List.exists (function
			| { psig = { snorm = LMethod(_,ret,[v]) } } ->
				flags := if !is_interface then
					(HExtends( raw_type_path ctx ([],"ArrayAccess") [ TPType (convert_signature ctx p ret) ]) :: !flags)
				else
					(HImplements( raw_type_path ctx ([],"ArrayAccess") [ TPType (convert_signature ctx p ret) ]) :: !flags);
				true
			| _ -> false) ilcls.cprops);

			let fields = ref [] in
			let run_fields fn f =
				List.iter (fun f ->
					try
						fields := fn f :: !fields
					with
						| Exit -> ()
				) f
			in
			let meths = if !is_interface then
					List.filter (fun m -> m.moverride = None) ilcls.cmethods
				else
					ilcls.cmethods
			in
			run_fields (convert_ilmethod ctx p) meths;
			run_fields (convert_ilfield ctx p) ilcls.cfields;
			run_fields (convert_ilprop ctx p) ilcls.cprops;

			let params = List.map (fun p ->
				{
					tp_name = "T" ^ string_of_int p.tnumber;
					tp_params = [];
					tp_constraints = [];
				}) ilcls.ctypes
			in
			let _, c = netpath_to_hx ctx.nstd ilcls.cpath in
			EClass {
				d_name = netname_to_hx c;
				d_doc = None;
				d_params = params;
				d_meta = !meta;
				d_flags = !flags;
				d_data = !fields;
			}

type il_any_field =
	| IlField of ilfield
	| IlMethod of ilmethod
	| IlProp of ilprop

let get_fname = function
	| IlField f -> f.fname
	| IlMethod m -> m.mname
	| IlProp p -> p.pname

let is_static = function
	| IlField f ->
		List.mem CStatic f.fflags.ff_contract
	| IlMethod m ->
		List.mem CMStatic m.mflags.mf_contract
	| IlProp { pmflags = Some m } ->
		List.mem CMStatic m.mf_contract
	| _ -> false

let change_name name = function
	| IlField f -> IlField { f with fname = name }
	| IlMethod m -> IlMethod { m with mname = name }
	| IlProp p -> IlProp { p with pname = name }

let compatible_methods m1 m2 = match m1,m2 with
	| IlMethod { msig = { snorm = LMethod(_,ret1,args1) } }, IlMethod { msig = { snorm = LMethod(_,ret2,args2) } } ->
		ret1 = ret2 && args1 = args2
	| _ -> false

let ilcls_from_ilsig ctx ilsig =
	let path, params = match ilsig with
		| LClass(path, params) | LValueType(path, params) ->
			path, params
		| LObject ->
			(["System"],[],"Object"),[]
		| _ -> raise Not_found (* all other types won't appear as superclass *)
	in
	match lookup_ilclass ctx.nstd ctx.ncom path with
	| None -> raise Not_found
	| Some c ->
		c, params

let rec ilapply_params params = function
	| LManagedPointer s -> LManagedPointer (ilapply_params params s)
	| LPointer s -> LPointer (ilapply_params params s)
	| LValueType (p,pl) -> LValueType(p, List.map (ilapply_params params) pl)
	| LClass (p,pl) -> LClass(p, List.map (ilapply_params params) pl)
	| LTypeParam i ->
		List.nth params i (* TODO: maybe i - 1? *)
	| LVector s -> LVector (ilapply_params params s)
	| LArray (s,a) -> LArray (ilapply_params params s, a)
	| LMethod (c,r,args) -> LMethod (c, ilapply_params params r, List.map (ilapply_params params) args)
	| p -> p

let ilcls_with_params ctx cls params =
	match cls.ctypes with
	| [] -> cls
	| _ ->
		{ cls with
			cfields = List.map (fun f -> { f with fsig = { f.fsig with snorm = ilapply_params params f.fsig.snorm } }) cls.cfields;
			cmethods = List.map (fun m -> { m with msig = { m.msig with snorm = ilapply_params params m.msig.snorm } }) cls.cmethods;
			cprops = List.map (fun p -> { p with psig = { p.psig with snorm = ilapply_params params p.psig.snorm } }) cls.cprops;
			csuper = Option.map (fun s -> { s with snorm = ilapply_params params s.snorm } ) cls.csuper;
			cimplements = List.map (fun s -> { s with snorm = ilapply_params params s.snorm } ) cls.cimplements;
		}

let compatible_methods m1 m2 = match m1, m2 with
	| LMethod(_,r1,a1), LMethod(_,r2,a2) -> a1 = a2
	| _ -> false

let compatible_field f1 f2 = match f1, f2 with
	| IlMethod { msig = { snorm = LMethod(_,_,a1) } },
	  IlMethod { msig = { snorm = LMethod(_,_,a2) } } ->
			a1 = a2
	| IlProp p1, IlProp p2 ->
			(* p1.psig.snorm = p2.psig.snorm *)
			true
	| IlField f1, IlField f2 ->
			(* f1.fsig.snorm = f2.fsig.snorm *)
			true
	| _ -> false

let get_all_fields cls =
	let all_fields = List.map (fun f -> IlField f, cls.cpath, f.fname, List.mem CStatic f.fflags.ff_contract) cls.cfields in
	let all_fields = all_fields @ List.map (fun m -> IlMethod m, cls.cpath, m.mname, List.mem CMStatic m.mflags.mf_contract) cls.cmethods in
	let all_fields = all_fields @ List.map (function
		| ({ pmflags = Some m } as p) ->
			IlProp p, cls.cpath, p.pname, List.mem CMStatic m.mf_contract
		| p ->
			IlProp p, cls.cpath, p.pname, false
	) cls.cprops in
	all_fields

let normalize_ilcls ctx cls =
	(* first filter out overloaded fields of same signature *)
	let rec loop acc = function
		| [] -> acc
		| m :: cmeths ->
			let static = List.mem CMStatic m.mflags.mf_contract in
			if List.exists (fun m2 -> m.mname = m2.mname && List.mem CMStatic m2.mflags.mf_contract = static && compatible_methods m.msig.snorm m2.msig.snorm) cmeths then
				loop acc cmeths
			else
				loop (m :: acc) cmeths
	in
	let meths = loop [] cls.cmethods in
	(* fix overrides *)
	(* get only the methods that aren't declared as override, but may be *)
	let meths = List.map (fun v -> ref v) meths in
	let no_overrides = List.filter (fun m ->
		let m = !m in
		not (List.mem CMStatic m.mflags.mf_contract)
	) meths in
	let no_overrides = ref no_overrides in

	let all_fields = ref [] in
	let rec loop cls = try
		match cls.csuper with
		| Some { snorm = LClass((["System"],[],"Object"),_) }
		| Some { snorm = LObject } | None -> ()
		| Some s ->
			let cls, params = ilcls_from_ilsig ctx s.snorm in
			let cls = ilcls_with_params ctx cls params in
			no_overrides := List.filter (fun v ->
				let m = !v in
				let is_override_here = List.exists (fun m2 ->
					m2.mname = m.mname && not (List.mem CMStatic m2.mflags.mf_contract) && compatible_methods m.msig.snorm m2.msig.snorm
				) cls.cmethods in
				if is_override_here then v := { m with moverride = Some(cls.cpath, m.mname) };
				not is_override_here
			) !no_overrides;
			all_fields := get_all_fields cls @ !all_fields;
			loop cls
		with | Not_found -> ()
	in
	loop cls;
	List.iter (fun v -> v := { !v with moverride = None }) !no_overrides;
	let added = ref [] in

	let current_all = ref (get_all_fields cls @ !all_fields) in
	(* look for interfaces and add missing implementations (some methods' implementation is optional) *)
	let rec loop_interface cls iface = try
		match iface.snorm with
		| LClass((["System"],[],"Object"),_) | LObject -> ()
		| LClass(path,_) when path = cls.cpath -> ()
		| s ->
			let cif, params = ilcls_from_ilsig ctx s in
			let cif = ilcls_with_params ctx cif params in
			List.iter (function
				| (f,_,name,false) as ff ->
					(* look for compatible fields *)
					if not (List.exists (function
						| (f2,_,name2,false) when name = name2 ->
							compatible_field f f2
						| _ -> false
					) !current_all) then begin
						current_all := ff :: !current_all;
						added := ff :: !added
					end
				| _ -> ()
			) (get_all_fields cif);
			List.iter (loop_interface cif) cif.cimplements
		with | Not_found -> ()
	in
	List.iter (loop_interface cls) cls.cimplements;

	(* filter out properties that were already declared *)
	let props = List.filter (function
		| ({ pmflags = Some m } as p) ->
			let static = List.mem CMStatic m.mf_contract in
			let name = p.pname in
			not (List.exists (function (IlProp _,_,n,s) -> s = static && name = n | _ -> false) !all_fields)
		| _ -> false
	) cls.cprops in
	let cls = { cls with cmethods = List.map (fun v -> !v) meths; cprops = props } in

	let clsfields = !added @ get_all_fields cls in
	let super_fields = !all_fields in
	all_fields := clsfields @ !all_fields;
	let refclsfields = (List.map (fun v -> ref v) clsfields) in
  (* search static / non-static name clash *)
  (* change field name to not collide with haxe keywords *)
  let iter_field v =
		let f, p, name, is_static = !v in
    let change = match name with
    | "callback" | "cast" | "extern" | "function" | "in" | "typedef" | "using" | "var" | "untyped" | "inline" -> true
    | _ ->
			(is_static && List.exists (function | (f,_,n,false) -> name = n | _ -> false) !all_fields) ||
			not is_static && match f with (* filter methods that have the same name as fields *)
			| IlMethod _ ->
				List.exists (function | ( (IlProp _ | IlField _),_,n,false) -> name = n | _ -> false) super_fields ||
				List.exists (function | ( (IlProp _ | IlField _),_,n,s) -> name = n | _ -> false) clsfields
			| _ -> false
    in
    if change then
			let name = "%" ^ name in
			v := change_name name f, p, name, is_static
  in
	List.iter iter_field refclsfields;

	let clsfields = List.map (fun v -> !v) refclsfields in
	let fields = List.filter (function | (IlField _,_,_,_) -> true | _ -> false) clsfields in
	let methods = List.filter (function | (IlMethod _,_,_,_) -> true | _ -> false) clsfields in
	let props = List.filter (function | (IlProp _,_,_,_) -> true | _ -> false) clsfields in
	{ cls with
		cfields = List.map (function | (IlField f,_,_,_) -> f | _ -> assert false) fields;
		cmethods = List.map (function | (IlMethod f,_,_,_) -> f | _ -> assert false) methods;
		cprops = List.map (function | (IlProp f,_,_,_) -> f | _ -> assert false) props;
	}

let add_net_std com file =
	com.net_std <- file :: com.net_std

let add_net_lib com file std =
	let ilctx = ref None in
	let netpath_to_hx = netpath_to_hx std in
	let real_file = ref file in
	let get_ctx () =
		match !ilctx with
		| Some c ->
			c
		| None ->
			let file = try Common.find_file com file with
				| Not_found -> try Common.find_file com (file ^ ".dll") with
				| Not_found ->
					failwith (".NET lib " ^ file ^ " not found")
			in
			real_file := file;
			let r = PeReader.create_r (open_in_bin file) com.defines in
			let ctx = PeReader.read r in
			let clr_header = PeReader.read_clr_header ctx in
			let meta = IlMetaReader.read_meta_tables ctx clr_header in
			close_in (r.PeReader.ch);
			if PMap.mem "net_loader_debug" com.defines then
				print_endline ("for lib " ^ file);
			Hashtbl.iter (fun _ td ->
				let path = IlMetaTools.get_path (TypeDef td) in
				if PMap.mem "net_loader_debug" com.defines then
					Printf.printf "found %s\n" (path_s (netpath_to_hx path));
				Hashtbl.add com.net_path_map (netpath_to_hx path) path;
				Hashtbl.replace meta.il_typedefs path td
			) meta.il_typedefs;
			let meta = { nstd = std; ncom = com; nil = meta } in
			ilctx := Some meta;
			meta
	in

	let cache = Hashtbl.create 0 in
	let lookup path =
		try
			Hashtbl.find cache path
		with | Not_found -> try
			let ctx = get_ctx() in
			let ns, n, cl = hxpath_to_net ctx path in
			let cls = IlMetaTools.convert_class ctx.nil (ns,n,cl) in
			let cls = normalize_ilcls ctx cls in
			Hashtbl.add cache path (Some cls);
			Some cls
		with | Not_found ->
			Hashtbl.add cache path None;
			None
	in

	let all_files () =
		Hashtbl.fold (fun path _ acc -> match path with
			| _,_ :: _, _ -> acc
			| _ -> netpath_to_hx path :: acc) (get_ctx()).nil.il_typedefs []
	in

	let build path =
		let p = { pfile = !real_file ^ " @ " ^ path_s path; pmin = 0; pmax = 0; } in
		let pack = match fst path with | ["haxe";"root"] -> [] | p -> p in
		let cp = ref [] in
		let rec build path = try
			if PMap.mem "net_loader_debug" com.defines then
				Printf.printf "looking up %s\n" (path_s path);
			match lookup path with
			| Some cls ->
				let ctx = get_ctx() in
				let hxcls = convert_ilclass ctx p cls in
				cp := (hxcls,p) :: !cp;
				List.iter (fun ilpath ->
					let path = netpath_to_hx ilpath in
					build path
				) cls.cnested
			| _ -> ()
		with | Not_found | Exit ->
			()
		in
		build path;
		match !cp with
			| [] -> None
			| cp -> Some (!real_file, (pack,cp))
	in
	let build path p =
		build path
	in
  com.load_extern_type <- com.load_extern_type @ [build];
  com.net_libs <- (file, std, all_files, lookup) :: com.net_libs

let before_generate com =
	(* net version *)
	let net_ver = try
			int_of_string (PMap.find "net_ver" com.defines)
		with | Not_found ->
			Common.define_value com Define.NetVer "20";
			20
	in
	if net_ver < 20 then
		failwith (
			".NET version is defined to target .NET "
			^ string_of_int net_ver
			^ ", but the compiler can only output code to versions equal or superior to .NET 2.0 (defined as 20)"
		);
	let rec loop = function
		| v :: acc when v <= net_ver ->
			Common.raw_define com ("NET_" ^ string_of_int v);
			loop acc
		| _ -> ()
	in
	loop [20;21;30;35;40;45];

	(* net target *)
	let net_target = try
			String.lowercase (PMap.find "net_target" com.defines)
		with | Not_found ->
			"net"
	in
	Common.define_value com Define.NetTarget net_target;
	Common.raw_define com net_target;

	(* std dirs *)
	let stds = match com.net_std with
		| [] -> ["netlib"]
		| s -> s
	in
	(* look for all dirs that have the digraph NET_TARGET-NET_VER *)
	let digraph = net_target ^ "-" ^ string_of_int net_ver in
	let matched = ref [] in
	List.iter (fun f -> try
		let f = Common.find_file com (f ^ "/" ^ digraph) in
		matched := (f, Unix.opendir f) :: !matched
	with | _ -> ()) stds;

	if !matched = [] then failwith (
		"No .NET std lib directory with the pattern '" ^ digraph ^ "' was found in the -net-std search path. " ^
		"Try updating the hxcs lib to the latest version, or specifying another -net-std path.");
	List.iter (fun (path,f) ->
		let rec loop () =
			try
				let f = Unix.readdir f in
				let finsens = String.lowercase f in
				if String.ends_with finsens ".dll" then
					add_net_lib com (path ^ "/" ^ f) true;
				loop()
			with | End_of_file ->
				Unix.closedir f
		in
		loop()
	) !matched;

	(* now force all libraries to initialize *)
	List.iter (function (_,_,_,lookup) -> ignore (lookup ([],""))) com.net_libs
