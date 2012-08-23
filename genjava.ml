(*
 *  haXe/C# & Java Compiler
 *  Copyright (c)2011 Cauê Waneck
 *  based on and including code by (c)2005-2008 Nicolas Cannasse, Hugh Sanderson and Franco Ponticelli
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Ast
open Common
open Gencommon
open Gencommon.SourceWriter
open Type
open Printf
open Option
open ExtString

let is_boxed_type t = match follow t with
  | TInst ({ cl_path = (["java";"lang"], "Boolean") }, [])
  | TInst ({ cl_path = (["java";"lang"], "Double") }, [])
  | TInst ({ cl_path = (["java";"lang"], "Integer") }, [])
  | TInst ({ cl_path = (["java";"lang"], "Byte") }, [])
  | TInst ({ cl_path = (["java";"lang"], "Short") }, [])
  | TInst ({ cl_path = (["java";"lang"], "Character") }, [])
  | TInst ({ cl_path = (["java";"lang"], "Float") }, []) -> true
  | _ -> false

let unboxed_type gen t tbyte tshort tchar tfloat = match follow t with
  | TInst ({ cl_path = (["java";"lang"], "Boolean") }, []) -> gen.gcon.basic.tbool
  | TInst ({ cl_path = (["java";"lang"], "Double") }, []) -> gen.gcon.basic.tfloat
  | TInst ({ cl_path = (["java";"lang"], "Integer") }, []) -> gen.gcon.basic.tint
  | TInst ({ cl_path = (["java";"lang"], "Byte") }, []) -> tbyte
  | TInst ({ cl_path = (["java";"lang"], "Short") }, []) -> tshort
  | TInst ({ cl_path = (["java";"lang"], "Character") }, []) -> tchar
  | TInst ({ cl_path = (["java";"lang"], "Float") }, []) -> tfloat
  | _ -> assert false

let rec t_has_type_param t = match follow t with
  | TInst({ cl_kind = KTypeParameter _ }, []) -> true
  | TEnum(_, params)
  | TInst(_, params) -> List.exists t_has_type_param params
  | TFun(f,ret) -> t_has_type_param ret || List.exists (fun (_,_,t) -> t_has_type_param t) f
  | _ -> false

let rec t_has_type_param_shallow last t = match follow t with
  | TInst({ cl_kind = KTypeParameter _ }, []) -> true
  | TEnum(_, params)
  | TInst(_, params) when not last -> List.exists (t_has_type_param_shallow true) params
  | TFun(f,ret) when not last -> t_has_type_param_shallow true ret  || List.exists (fun (_,_,t) -> t_has_type_param_shallow true t) f
  | _ -> false

let is_java_basic_type t =
  match follow t with
    | TInst( { cl_path = (["haxe"], "Int32") }, [] )
    | TInst( { cl_path = (["haxe"], "Int64") }, [] )
    | TInst( { cl_path = ([], "Int") }, [] )
    | TInst( { cl_path = ([], "Float") }, [] )
    | TEnum( { e_path = ([], "Bool") }, [] ) ->
      true
    | _ -> false

let is_bool t =
  match follow t with
    | TEnum( { e_path = ([], "Bool") }, [] ) ->
      true
    | _ -> false

let is_int_float gen t =
  match follow (gen.greal_type t) with
    | TInst( { cl_path = (["haxe"], "Int64") }, [] )
    | TInst( { cl_path = (["haxe"], "Int32") }, [] )
    | TInst( { cl_path = ([], "Int") }, [] )
    | TInst( { cl_path = ([], "Float") }, [] ) ->
      true
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
(* JavaSpecificESynf *)
(* ******************************************* *)

(*

  Some Java-specific syntax filters that must run before ExpressionUnwrap

  dependencies:
    It must run before ExprUnwrap, as it may not return valid Expr/Statement expressions
    It must run before ClassInstance, as it will detect expressions that need unchanged TTypeExpr
    It must run after CastDetect, as it changes casts
    It must run after TryCatchWrapper, to change Std.is() calls inside there

*)
module JavaSpecificESynf =
struct

  let name = "java_specific_e"

  let priority = solve_deps name [ DBefore ExpressionUnwrap.priority; DBefore ClassInstance.priority; DAfter CastDetect.priority; DAfter TryCatchWrapper.priority ]

  let get_cl_from_t t =
    match follow t with
      | TInst(cl,_) -> cl
      | _ -> assert false

  let traverse gen runtime_cl =
    let basic = gen.gcon.basic in
    let float_cl = get_cl ( get_type gen (["java";"lang"], "Double")) in
    let bool_md = get_type gen (["java";"lang"], "Boolean") in

    let is_var = alloc_var "__is__" t_dynamic in

    let rec run e =
      match e.eexpr with
        (* Math changes *)
        | TField( { eexpr = TTypeExpr( TClassDecl( { cl_path = (["java";"lang"], "Math") }) ) }, "NaN" ) ->
          mk_static_field_access_infer float_cl "NaN" e.epos []
        | TField( { eexpr = TTypeExpr( TClassDecl( { cl_path = (["java";"lang"], "Math") }) ) }, "NEGATIVE_INFINITY" ) ->
          mk_static_field_access_infer float_cl "NEGATIVE_INFINITY" e.epos []
        | TField( { eexpr = TTypeExpr( TClassDecl( { cl_path = (["java";"lang"], "Math") }) ) }, "POSITIVE_INFINITY" ) ->
          mk_static_field_access_infer float_cl "POSITIVE_INFINITY" e.epos []
        | TField( { eexpr = TTypeExpr( TClassDecl( { cl_path = (["java";"lang"], "Math") }) ) }, "isNaN" ) ->
          mk_static_field_access_infer float_cl "isNaN" e.epos []
        | TCall( { eexpr = TField( { eexpr = TTypeExpr( TClassDecl( { cl_path = (["java";"lang"], "Math") }) ) }, "floor" ) }, _)
        | TCall( { eexpr = TField( { eexpr = TTypeExpr( TClassDecl( { cl_path = (["java";"lang"], "Math") }) ) }, "round" ) }, _)
        | TCall( { eexpr = TField( { eexpr = TTypeExpr( TClassDecl( { cl_path = (["java";"lang"], "Math") }) ) }, "ceil" ) }, _) ->
          mk_cast basic.tint (Type.map_expr run e)
        | TCall( ( { eexpr = TField( { eexpr = TTypeExpr( TClassDecl( { cl_path = (["java";"lang"], "Math") }) ) }, "isFinite" ) } as efield ), [v]) ->
          { e with eexpr =
            TUnop(Ast.Not, Ast.Prefix, {
              e with eexpr = TCall( mk_static_field_access_infer float_cl "isInfinite" efield.epos [], [run v] )
            })
          }
        (* end of math changes *)

        (* Std.is() *)
        | TCall(
            { eexpr = TField( { eexpr = TTypeExpr ( TClassDecl { cl_path = ([], "Std") } ) }, "is") },
            [ obj; { eexpr = TTypeExpr(md) } ]
          ) ->
          let mk_is obj md =
            { e with eexpr = TCall( { eexpr = TLocal is_var; etype = t_dynamic; epos = e.epos }, [
              run obj;
              { eexpr = TTypeExpr md; etype = t_dynamic (* this is after all a syntax filter *); epos = e.epos }
            ] ) }
          in
          (match follow_module follow md with
            | TClassDecl({ cl_path = ([], "Float") }) ->
              {
                eexpr = TCall(
                  mk_static_field_access_infer runtime_cl "isDouble" e.epos [],
                  [ run obj ]
                );
                etype = basic.tbool;
                epos = e.epos
              }
            | TClassDecl{ cl_path = ([], "Int") } ->
              {
                eexpr = TCall(
                  mk_static_field_access_infer runtime_cl "isInt" e.epos [],
                  [ run obj ]
                );
                etype = basic.tbool;
                epos = e.epos
              }
            | TEnumDecl{ e_path = ([], "Bool") } ->
              mk_is obj bool_md
            | TClassDecl{ cl_path = ([], "Dynamic") } ->
              (match obj.eexpr with
                | TLocal _ | TConst _ -> { e with eexpr = TConst(TBool true) }
                | _ -> { e with eexpr = TBlock([run obj; { e with eexpr = TConst(TBool true) }]) }
              )
            | _ ->
              mk_is obj md
          )
        (* end Std.is() *)
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;


(* ******************************************* *)
(* JavaSpecificSynf *)
(* ******************************************* *)

(*

  Some Java-specific syntax filters that can run after ExprUnwrap

  dependencies:
    Runs after ExprUnwarp

*)

module JavaSpecificSynf =
struct

  let name = "java_specific"

  let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority; DBefore IntDivisionSynf.priority ]

  let java_hash s =
    let h = ref Int32.zero in
    let thirtyone = Int32.of_int 31 in
    for i = 0 to String.length s - 1 do
      h := Int32.add (Int32.mul thirtyone !h) (Int32.of_int (int_of_char (String.unsafe_get s i)));
    done;
    !h

  let rec is_final_return_expr is_switch e =
    let is_final_return_expr = is_final_return_expr is_switch in
    match e.eexpr with
      | TReturn _
      | TThrow _ -> true
      (* this is hack to not use 'break' on switch cases *)
      | TLocal { v_name = "__fallback__" } when is_switch -> true
      | TCall( { eexpr = TLocal { v_name = "__goto__" } }, _ ) -> true
      | TParenthesis p -> is_final_return_expr p
      | TBlock bl -> is_final_return_block is_switch bl
      | TSwitch (_, el_e_l, edef) ->
        List.for_all (fun (_,e) -> is_final_return_expr e) el_e_l && Option.map_default is_final_return_expr false edef
      | TMatch (_, _, il_vl_e_l, edef) ->
        List.for_all (fun (_,_,e) -> is_final_return_expr e)il_vl_e_l && Option.map_default is_final_return_expr false edef
      | TIf (_,eif, Some eelse) ->
        is_final_return_expr eif && is_final_return_expr eelse
      | TFor (_,_,e) ->
        is_final_return_expr e
      | TWhile (_,e,_) ->
        is_final_return_expr e
      | TFunction tf ->
        is_final_return_expr tf.tf_expr
      | TTry (e, ve_l) ->
        is_final_return_expr e && List.for_all (fun (_,e) -> is_final_return_expr e) ve_l
      | _ -> false

  and is_final_return_block is_switch el =
    match el with
      | [] -> false
      | final :: [] -> is_final_return_expr is_switch final
      | hd :: tl -> is_final_return_block is_switch tl

  let is_null e = match e.eexpr with | TConst(TNull) -> true | _ -> false

  let rec is_equatable gen t =
    match follow t with
      | TInst(cl,_) ->
        if cl.cl_path = (["haxe";"lang"], "IEquatable") then
          true
        else
          List.exists (fun (cl,p) -> is_equatable gen (TInst(cl,p))) cl.cl_implements
            || (match cl.cl_super with | Some(cl,p) -> is_equatable gen (TInst(cl,p)) | None -> false)
      | _ -> false

  (*
    Changing string switch
    will take an expression like
    switch(str)
    {
      case "a":
      case "b":
    }

    and modify it to:
    {
      var execute_def = true;
      switch(str.hashCode())
      {
        case (hashcode of a):
          if (str == "a")
          {
            execute_def = false;
            ..code here
          } //else if (str == otherVariableWithSameHashCode) {
            ...
          }
        ...
      }
      if (execute_def)
      {
        ..default code
      }
    }

    this might actually be slower in some cases than a if/else approach, but it scales well and as a bonus,
    hashCode in java are cached, so we only have the performance hit once to cache it.
  *)
  let change_string_switch gen eswitch e1 ecases edefault =
    let basic = gen.gcon.basic in
    let is_final_ret = is_final_return_expr false eswitch in

    let has_default = is_some edefault in
    let block = ref [] in
    let local = match e1.eexpr with
      | TLocal _ -> e1
      | _ ->
        let var = mk_temp gen "svar" e1.etype in
        let added = { e1 with eexpr = TVars([var, Some(e1)]); etype = basic.tvoid } in
        let local = mk_local var e1.epos in
        block := added :: !block;
        local
    in
    let execute_def_var = mk_temp gen "executeDef" gen.gcon.basic.tbool in
    let execute_def = mk_local execute_def_var e1.epos in
    let execute_def_set = { eexpr = TBinop(Ast.OpAssign, execute_def, { eexpr = TConst(TBool false); etype = basic.tbool; epos = e1.epos }); etype = basic.tbool; epos = e1.epos } in

    let hash_cache = ref None in

    let local_hashcode = ref { local with
      eexpr = TCall({ local with
        eexpr = TField(local, "hashCode");
        etype = TFun([], basic.tint);
      }, []);
      etype = basic.tint
    } in

    let get_hash_cache () =
      match !hash_cache with
        | Some c -> c
        | None ->
          let var = mk_temp gen "hash" basic.tint in
          let cond = !local_hashcode in
          block := { eexpr = TVars([var, Some cond]); etype = basic.tvoid; epos = local.epos } :: !block;
          let local = mk_local var local.epos in
          local_hashcode := local;
          hash_cache := Some local;
          local
    in

    let has_case = ref false in
    (* first we need to reorder all cases so all collisions are close to each other *)

    let get_str e = match e.eexpr with | TConst(TString s) -> s | _ -> assert false in
    let has_conflict = ref false in

    let rec reorder_cases unordered ordered =
      match unordered with
        | [] -> ordered
        | (el, e) :: tl ->
          let current = Hashtbl.create 1 in
          List.iter (fun e ->
            let str = get_str e in
            let hash = java_hash str in
            Hashtbl.add current hash true
          ) el;

          let rec extract_fields cases found_cases ret_cases =
            match cases with
              | [] -> found_cases, ret_cases
              | (el, e) :: tl ->
                if List.exists (fun e -> Hashtbl.mem current (java_hash (get_str e)) ) el then begin
                  has_conflict := true;
                  List.iter (fun e -> Hashtbl.add current (java_hash (get_str e)) true) el;
                  extract_fields tl ( (el, e) :: found_cases ) ret_cases
                end else
                  extract_fields tl found_cases ( (el, e) :: ret_cases )
          in
          let found, remaining = extract_fields tl [] [] in
          let ret = if found <> [] then
            let ret = List.sort (fun (e1,_) (e2,_) -> compare (List.length e2) (List.length e1) ) ( (el, e) :: found ) in
            let rec loop ret acc =
              match ret with
                | (el, e) :: ( (_,_) :: _ as tl ) -> loop tl ( (true, el, e) :: acc )
                | (el, e) :: [] -> ( (false, el, e) :: acc )
                | _ -> assert false
            in
            List.rev (loop ret [])
          else
            (false, el, e) :: []
          in

          reorder_cases remaining (ordered @ ret)
    in

    let already_in_cases = Hashtbl.create 0 in
    let change_case (has_fallback, el, e) =
      let conds, el = List.fold_left (fun (conds,el) e ->
        has_case := true;
        match e.eexpr with
          | TConst(TString s) ->
            let hashed = java_hash s in
            let equals_test = {
              eexpr = TCall({ e with eexpr = TField(local, "equals"); etype = TFun(["obj",false,t_dynamic],basic.tbool) }, [ e ]);
              etype = basic.tbool;
              epos = e.epos
            } in

            let hashed_expr = { eexpr = TConst(TInt hashed); etype = basic.tint; epos = e.epos } in
            let hashed_exprs = if !has_conflict then begin
              if Hashtbl.mem already_in_cases hashed then
                el
              else begin
                Hashtbl.add already_in_cases hashed true;
                hashed_expr :: el
              end
            end else hashed_expr :: el in

            let conds = match conds with
              | None -> equals_test
              | Some c ->
                (*
                  if there is more than one case, we should test first if hash equals to the one specified.
                  This way we can save a heavier string compare
                *)
                let equals_test = mk_paren {
                  eexpr = TBinop(Ast.OpBoolAnd, { eexpr = TBinop(Ast.OpEq, get_hash_cache(), hashed_expr); etype = basic.tbool; epos = e.epos }, equals_test);
                  etype = basic.tbool;
                  epos = e.epos;
                } in

                { eexpr = TBinop(Ast.OpBoolOr, equals_test, c); etype = basic.tbool; epos = e1.epos }
            in

            Some conds, hashed_exprs
          | _ -> assert false
      ) (None,[]) el in
      let e = if has_default then Codegen.concat execute_def_set e else e in
      let e = if !has_conflict then Codegen.concat e { e with eexpr = TBreak; etype = basic.tvoid } else e in
      let e = {
        eexpr = TIf(get conds, e, None);
        etype = basic.tvoid;
        epos = e.epos
      } in

      let e = if has_fallback then { e with eexpr = TBlock([ e; mk_local (alloc_var "__fallback__" t_dynamic) e.epos]) } else e in

      (el, e)
    in

    let switch = { eswitch with
      eexpr = TSwitch(!local_hashcode, List.map change_case (reorder_cases ecases []), None);
    } in
    (if !has_case then begin
      (if has_default then block := { e1 with eexpr = TVars([execute_def_var, Some({ e1 with eexpr = TConst(TBool true); etype = basic.tbool })]); etype = basic.tvoid } :: !block);
      block := switch :: !block
    end);
    (match edefault with
      | None -> ()
      | Some edef when not !has_case ->
        block := edef :: !block
      | Some edef ->
        let eelse = if is_final_ret then Some { eexpr = TThrow { eexpr = TConst(TNull); etype = t_dynamic; epos = edef.epos }; etype = basic.tvoid; epos = edef.epos } else None in
        block := { edef with eexpr = TIf(execute_def, edef, eelse); etype = basic.tvoid } :: !block
    );
    { eswitch with eexpr = TBlock(List.rev !block) }


  let get_cl_from_t t =
    match follow t with
      | TInst(cl,_) -> cl
      | _ -> assert false

  let traverse gen runtime_cl =
    let basic = gen.gcon.basic in
    let tchar = match ( get_type gen (["java"], "Char16") ) with | TTypeDecl t -> TType(t,[]) | _ -> assert false in
    let tbyte = match ( get_type gen (["java"], "Int8") ) with | TTypeDecl t -> TType(t,[]) | _ -> assert false in
    let tshort = match ( get_type gen (["java"], "Int16") ) with | TTypeDecl t -> TType(t,[]) | _ -> assert false in
    let tsingle = match ( get_type gen ([], "Single") ) with | TTypeDecl t -> TType(t,[]) | _ -> assert false in
    let string_ext = get_cl ( get_type gen (["haxe";"lang"], "StringExt")) in

    let is_string t = match follow t with | TInst({ cl_path = ([], "String") }, []) -> true | _ -> false in

    let rec run e =
      match e.eexpr with
        (* for new NativeArray<T> issues *)
        | TNew(({ cl_path = (["java"], "NativeArray") } as cl), [t], el) when t_has_type_param t ->
          mk_cast (TInst(cl,[t])) (mk_cast t_dynamic ({ e with eexpr = TNew(cl, [t_empty], List.map run el) }))

        (* Std.int() *)
        | TCall(
            { eexpr = TField( { eexpr = TTypeExpr ( TClassDecl ({ cl_path = ([], "Std") }) ) }, "int") },
            [obj]
          ) ->
          run (mk_cast basic.tint obj)
        (* end Std.int() *)

        | TField( ef, "length" ) when is_string ef.etype ->
          { e with eexpr = TCall(Type.map_expr run e, []) }
        | TCall( ( { eexpr = TField({ eexpr = TTypeExpr (TTypeDecl t) }, "fromCharCode") } ), [cc] ) when is_string (follow (TType(t,List.map snd t.t_types))) ->
          { e with eexpr = TNew(get_cl_from_t basic.tstring, [], [mk_cast tchar (run cc); mk_int gen 1 cc.epos]) }
        | TCall( ( { eexpr = TField(ef, field) } as efield ), args ) when is_string ef.etype ->
          (match field with
            | "charAt" | "charCodeAt" | "split" | "indexOf"
            | "lastIndexOf" | "substring" | "substr" ->
              { e with eexpr = TCall(mk_static_field_access_infer string_ext field e.epos [], [run ef] @ (List.map run args)) }
            | _ when String.get field 0 = '_' ->
              { e with eexpr = TCall({ efield with eexpr = TField(run ef, String.sub field 1 ( (String.length field) - 1)) }, List.map run args) }
            | _ ->
              { e with eexpr = TCall(run efield, List.map run args) }
          )

        | TCast(expr, m) when is_boxed_type e.etype ->
          (* let unboxed_type gen t tbyte tshort tchar tfloat = match follow t with *)
          run { e with etype = unboxed_type gen e.etype tbyte tshort tchar tsingle }

        | TCast(expr, _) when is_bool e.etype ->
          {
            eexpr = TCall(
              mk_static_field_access_infer runtime_cl "toBool" expr.epos [],
              [ run expr ]
            );
            etype = basic.tbool;
            epos = e.epos
          }

        | TCast(expr, _) when is_int_float gen e.etype && not (is_int_float gen expr.etype) ->
          let needs_cast = match gen.gfollow#run_f e.etype with
            | TInst _ -> false
            | _ -> true
          in

          let fun_name = match follow e.etype with
            | TInst ({ cl_path = ([], "Float") },[]) -> "toDouble"
            | _ -> "toInt"
          in

          let ret = {
            eexpr = TCall(
              mk_static_field_access_infer runtime_cl fun_name expr.epos [],
              [ run expr ]
            );
            etype = if fun_name = "toDouble" then basic.tfloat else basic.tint;
            epos = expr.epos
          } in

          if needs_cast then mk_cast e.etype ret else ret

        (*| TCast(expr, c) when is_int_float gen e.etype ->
          (* cases when float x = (float) (java.lang.Double val); *)
          (* FIXME: this fix is broken since it will fail on cases where float x = (float) (java.lang.Float val) or similar. FIX THIS *)
          let need_second_cast = match gen.gfollow#run_f e.etype with
            | TInst _ -> false
            | _ -> true
          in
          if need_second_cast then { e with eexpr = TCast(mk_cast (follow e.etype) (run expr), c) }  else Type.map_expr run e*)
        | TCast(expr, _) when is_string e.etype ->
          { e with eexpr = TCall( mk_static_field_access_infer runtime_cl "toString" expr.epos [], [run expr] ) }

        | TSwitch(cond, ecases, edefault) when is_string cond.etype ->
          (*let change_string_switch gen eswitch e1 ecases edefault =*)
          change_string_switch gen e (run cond) (List.map (fun (el,e) -> (el, run e)) ecases) (Option.map run edefault)

        | TBinop( (Ast.OpNotEq as op), e1, e2)
        | TBinop( (Ast.OpEq as op), e1, e2) when not (is_null e2 || is_null e1) && (is_string e1.etype || is_string e2.etype || is_equatable gen e1.etype || is_equatable gen e2.etype) ->
          let static = mk_static_field_access_infer (runtime_cl) "valEq" e1.epos [] in
          let eret = { eexpr = TCall(static, [run e1; run e2]); etype = gen.gcon.basic.tbool; epos=e.epos } in
          if op = Ast.OpNotEq then { eret with eexpr = TUnop(Ast.Not, Ast.Prefix, eret) } else eret
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    (if java_hash "Testing string hashCode implementation from haXe" <> (Int32.of_int 545883604) then assert false);
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

let connecting_string = "?" (* ? see list here http://www.fileformat.info/info/unicode/category/index.htm and here for C# http://msdn.microsoft.com/en-us/library/aa664670.aspx *)
let default_package = "java" (* I'm having this separated as I'm still not happy with having a cs package. Maybe dotnet would be better? *)
let strict_mode = ref false (* strict mode is so we can check for unexpected information *)

(* reserved c# words *)
let reserved = let res = Hashtbl.create 120 in
  List.iter (fun lst -> Hashtbl.add res lst ("_" ^ lst)) ["abstract"; "assert"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class";
    "const"; "continue"; "default"; "do"; "double"; "else"; "enum"; "extends"; "final";
    "false"; "finally"; "float"; "for"; "goto"; "if"; "implements"; "import"; "instanceof"; "int";
    "interface"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "short";
    "static"; "strictfp"; "super"; "switch"; "synchronized"; "this"; "throw"; "throws"; "transient"; "true"; "try";
    "void"; "volatile"; "while"; ];
  res

let dynamic_anon = TAnon( { a_fields = PMap.empty; a_status = ref Closed } )

let rec get_class_modifiers meta cl_type cl_access cl_modifiers =
  match meta with
    | [] -> cl_type,cl_access,cl_modifiers
    (*| (":struct",[],_) :: meta -> get_class_modifiers meta "struct" cl_access cl_modifiers*)
    | (":protected",[],_) :: meta -> get_class_modifiers meta cl_type "protected" cl_modifiers
    | (":internal",[],_) :: meta -> get_class_modifiers meta cl_type "" cl_modifiers
    (* no abstract for now | (":abstract",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("abstract" :: cl_modifiers)
    | (":static",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("static" :: cl_modifiers) TODO: support those types *)
    | (":final",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("final" :: cl_modifiers)
    | _ :: meta -> get_class_modifiers meta cl_type cl_access cl_modifiers

let rec get_fun_modifiers meta access modifiers =
  match meta with
    | [] -> access,modifiers
    | (":protected",[],_) :: meta -> get_fun_modifiers meta "protected" modifiers
    | (":internal",[],_) :: meta -> get_fun_modifiers meta "" modifiers
    (*| (":readonly",[],_) :: meta -> get_fun_modifiers meta access ("readonly" :: modifiers)*)
    (*| (":unsafe",[],_) :: meta -> get_fun_modifiers meta access ("unsafe" :: modifiers)*)
    | (":volatile",[],_) :: meta -> get_fun_modifiers meta access ("volatile" :: modifiers)
    | (":transient",[],_) :: meta -> get_fun_modifiers meta access ("transient" :: modifiers)
    | _ :: meta -> get_fun_modifiers meta access modifiers

(* this was the way I found to pass the generator context to be accessible across all functions here *)
(* so 'configure' is almost 'top-level' and will have all functions needed to make this work *)
let configure gen =
  let basic = gen.gcon.basic in

  let fn_cl = get_cl (get_type gen (["haxe";"lang"],"Function")) in

  let runtime_cl = get_cl (get_type gen (["haxe";"lang"],"Runtime")) in

  (*let string_ref = get_cl ( get_type gen (["haxe";"lang"], "StringRefl")) in*)

  let ti64 = match ( get_type gen (["haxe";"_Int64"], "NativeInt64") ) with | TTypeDecl t -> TType(t,[]) | _ -> assert false in

  let has_tdynamic params =
    List.exists (fun e -> match gen.greal_type e with | TDynamic _ -> true | _ -> false) params
  in

  (*
    The type parameters always need to be changed to their boxed counterparts
  *)
  let change_param_type md params =
    match md with
      | TClassDecl( { cl_path = (["java"], "NativeArray") } ) -> params
      | _ ->
        match params with
          | [] -> []
          | _ ->
            if has_tdynamic params then List.map (fun _ -> t_dynamic) params else
              List.map (fun t ->
                let f_t = gen.gfollow#run_f t in
                match gen.gfollow#run_f t with
                  | TEnum ({ e_path = ([], "Bool") }, [])
                  | TInst ({ cl_path = ([],"Float") },[])
                  | TInst ({ cl_path = ["haxe"],"Int32" },[])
                  | TInst ({ cl_path = ([],"Int") },[])
                  | TType ({ t_path = ["haxe";"_Int64"], "NativeInt64" },[])
                  | TType ({ t_path = ["haxe"],"Int64" },[])
                  | TType ({ t_path = ["java"],"Int8" },[])
                  | TType ({ t_path = ["java"],"Int16" },[])
                  | TType ({ t_path = ["java"],"Char16" },[])
                  | TType ({ t_path = [],"Single" },[]) -> basic.tnull f_t
                  (*| TType ({ t_path = [], "Null"*)
                  | TInst (cl, ((_ :: _) as p)) ->
                    TInst(cl, List.map (fun _ -> t_dynamic) p)
                  | TEnum (e, ((_ :: _) as p)) ->
                    TEnum(e, List.map (fun _ -> t_dynamic) p)
                  | _ -> t
              ) params
  in

  let rec change_ns ns = match ns with
    | [] -> ["haxe"; "root"]
    | _ -> ns
  in

  let change_clname n = n in

  let change_id name = try Hashtbl.find reserved name with | Not_found -> name in

  let change_field = change_id in

  let write_id w name = write w (change_id name) in

  let write_field w name = write w (change_field name) in

  gen.gfollow#add ~name:"follow_basic" (fun t -> match t with
      | TEnum ({ e_path = ([], "Bool") }, [])
      | TEnum ({ e_path = ([], "Void") }, [])
      | TInst ({ cl_path = ([],"Float") },[])
      | TInst ({ cl_path = ([],"Int") },[])
      | TInst( { cl_path = (["haxe"], "Int32") }, [] )
      | TInst( { cl_path = (["haxe"], "Int64") }, [] )
      | TType ({ t_path = ["haxe";"_Int64"], "NativeInt64" },[])
      | TType ({ t_path = ["java"],"Int8" },[])
      | TType ({ t_path = ["java"],"Int16" },[])
      | TType ({ t_path = ["java"],"Char16" },[])
      | TType ({ t_path = [],"Single" },[])
      | TType ({ t_path = [],"Null" },[_]) -> Some t
			| TInst( { cl_path = ([], "EnumValue") }, _  ) -> Some t_dynamic
      | _ -> None);

  let change_path path = (change_ns (fst path), change_clname (snd path)) in

  let path_s path = match path with
    | (ns,clname) -> path_s (change_ns ns, change_clname clname)
  in

  let cl_cl = get_cl (get_type gen (["java";"lang"],"Class")) in

  let rec real_type t =
    let t = gen.gfollow#run_f t in
    match t with
      | TInst( { cl_path = (["haxe"], "Int32") }, [] ) -> gen.gcon.basic.tint
      | TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> ti64
      | TInst( { cl_path = ([], "Class") }, p  )
      | TInst( { cl_path = ([], "Enum") }, p  ) -> TInst(cl_cl,[t_dynamic])
      | TEnum _
      | TInst _ -> t
      | TType({ t_path = ([], "Null") }, [t]) when is_java_basic_type t -> t_dynamic
      | TType({ t_path = ([], "Null") }, [t]) ->
        (match follow t with
          | TInst( { cl_kind = KTypeParameter _ }, []) -> t_dynamic
          | _ -> real_type t
        )
      | TType _ -> t
      | TAnon (anon) when (match !(anon.a_status) with | Statics _ | EnumStatics _ -> true | _ -> false) -> t
      | TAnon _ -> dynamic_anon
      | TFun _ -> TInst(fn_cl,[])
      | _ -> t_dynamic
  in

  let is_dynamic t = match real_type t with
    | TMono _ | TDynamic _ -> true
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
      | TEnum ({ e_path = ([], "Bool") }, []) -> "boolean"
      | TEnum ({ e_path = ([], "Void") }, []) -> "java.lang.Object"
      | TInst ({ cl_path = ([],"Float") },[]) -> "double"
      | TInst ({ cl_path = ([],"Int") },[]) -> "int"
      | TType ({ t_path = ["haxe";"_Int64"], "NativeInt64" },[]) -> "long"
      | TType ({ t_path = ["java"],"Int8" },[]) -> "byte"
      | TType ({ t_path = ["java"],"Int16" },[]) -> "short"
      | TType ({ t_path = ["java"],"Char16" },[]) -> "char"
      | TType ({ t_path = [],"Single" },[]) -> "float"
      | TInst ({ cl_path = ["haxe"],"Int32" },[]) -> "int"
      | TInst ({ cl_path = ["haxe"],"Int64" },[]) -> "long"
      | TInst ({ cl_path = ([], "Dynamic") }, _) -> "java.lang.Object"
      | TInst({ cl_path = (["java"], "NativeArray") }, [param]) ->
        let rec check_t_s t =
          match real_type t with
            | TInst({ cl_path = (["java"], "NativeArray") }, [param]) ->
              (check_t_s param) ^ "[]"
            | _ -> t_s (run_follow gen t)
        in
        (check_t_s param) ^ "[]"
      (* end of basic types *)
      | TInst ({ cl_kind = KTypeParameter _; cl_path=p }, []) -> snd p
      | TMono r -> (match !r with | None -> "java.lang.Object" | Some t -> t_s (run_follow gen t))
      | TInst ({ cl_path = [], "String" }, []) -> "java.lang.String"
      | TInst ({ cl_path = [], "Class" }, _) | TInst ({ cl_path = [], "Enum" }, _) -> assert false (* should have been converted earlier *)
      | TEnum (({e_path = p;} as e), params) -> (path_param_s (TEnumDecl e) p params)
      | TInst (({cl_path = p;} as cl), params) -> (path_param_s (TClassDecl cl) p params)
      | TType (({t_path = p;} as t), params) -> (path_param_s (TTypeDecl t) p params)
      | TAnon (anon) ->
        (match !(anon.a_status) with
          | Statics _ | EnumStatics _ -> "java.lang.Class"
          | _ -> "java.lang.Object")
      | TDynamic _ -> "java.lang.Object"
      (* No Lazy type nor Function type made. That's because function types will be at this point be converted into other types *)
      | _ -> if !strict_mode then begin trace ("[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"); assert false end else "[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"

  and param_t_s t =
    match run_follow gen t with
      | TEnum ({ e_path = ([], "Bool") }, []) -> "java.lang.Boolean"
      | TInst ({ cl_path = ([],"Float") },[]) -> "java.lang.Double"
      | TInst ({ cl_path = ([],"Int") },[]) -> "java.lang.Integer"
      | TType ({ t_path = ["haxe";"_Int64"], "NativeInt64" },[]) -> "java.lang.Long"
      | TInst ({ cl_path = ["haxe"],"Int64" },[]) -> "java.lang.Long"
      | TInst ({ cl_path = ["haxe"],"Int32" },[]) -> "java.lang.Integer"
      | TType ({ t_path = ["java"],"Int8" },[]) -> "java.lang.Byte"
      | TType ({ t_path = ["java"],"Int16" },[]) -> "java.lang.Short"
      | TType ({ t_path = ["java"],"Char16" },[]) -> "java.lang.Character"
      | TType ({ t_path = [],"Single" },[]) -> "java.lang.Float"
      | TDynamic _ -> "?"
      | TInst (cl, params) -> t_s (TInst(cl, change_param_type (TClassDecl cl) params))
      | TType (cl, params) -> t_s (TType(cl, change_param_type (TTypeDecl cl) params))
      | TEnum (e, params) -> t_s (TEnum(e, change_param_type (TEnumDecl e) params))
      | _ -> t_s t

  and path_param_s md path params =
      match params with
        | [] -> path_s path
        | _ when has_tdynamic params -> path_s path
        | _ -> sprintf "%s<%s>" (path_s path) (String.concat ", " (List.map (fun t -> param_t_s t) (change_param_type md params)))
  in

  let rett_s t =
    match t with
      | TEnum ({e_path = ([], "Void")}, []) -> "void"
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
      | c when c < 32 || c >= 127 -> Buffer.add_string b (Printf.sprintf "\\u%.4x" c)
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
      | TLocal { v_name = "__fallback__" }
      | TCall ({ eexpr = TLocal( { v_name = "__label__" } ) }, [ { eexpr = TConst(TInt _) } ] ) -> false
      | TBlock _ | TFor _ | TSwitch _ | TMatch _ | TTry _ | TIf _ -> false
      | TWhile (_,_,flag) when flag = Ast.NormalWhile -> false
      | _ -> true
  in

  let in_value = ref false in

  let rec md_s md =
    let md = follow_module (gen.gfollow#run_f) md in
    match md with
      | TClassDecl (cl) ->
        t_s (TInst(cl,[]))
      | TEnumDecl (e) ->
        t_s (TEnum(e,[]))
      | TTypeDecl t ->
        t_s (TType(t, []))
  in

  (*
    it seems that Java doesn't like when you create a new array with the type parameter defined
    so we'll just ignore all type parameters, and hope for the best!
  *)
  let rec transform_nativearray_t t = match real_type t with
    | TInst( ({ cl_path = (["java"], "NativeArray") } as narr), [t]) ->
      TInst(narr, [transform_nativearray_t t])
    | TInst(cl, params) -> TInst(cl, List.map (fun _ -> t_dynamic) params)
    | TEnum(e, params) -> TEnum(e, List.map (fun _ -> t_dynamic) params)
    | TType(t, params) -> TType(t, List.map (fun _ -> t_dynamic) params)
    | _ -> t
  in

  let expr_s w e =
    in_value := false;
    let rec expr_s w e =
      let was_in_value = !in_value in
      in_value := true;
      match e.eexpr with
        | TConst c ->
          (match c with
            | TInt i32 ->
              print w "%ld" i32;
              (match real_type e.etype with
                | TType( { t_path = (["haxe";"_Int64"], "NativeInt64") }, [] ) -> write w "L";
                | _ -> ()
              )
            | TFloat s ->
              write w s;
              (* fix for Int notation, which only fit in a Float *)
              (if not (String.contains s '.' || String.contains s 'e' || String.contains s 'E') then write w ".0");
              (match real_type e.etype with
                | TType( { t_path = ([], "Single") }, [] ) -> write w "f"
                | _ -> ()
              )
            | TString s -> print w "\"%s\"" (escape s)
            | TBool b -> write w (if b then "true" else "false")
            | TNull ->
              (match real_type e.etype with
                | TType( { t_path = (["haxe";"_Int64"], "NativeInt64") }, [] )
                | TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> write w "0L"
                | TInst( { cl_path = (["haxe"], "Int32") }, [] )
                | TInst({ cl_path = ([], "Int") },[]) -> expr_s w ({ e with eexpr = TConst(TInt Int32.zero) })
                | TInst({ cl_path = ([], "Float") },[]) -> expr_s w ({ e with eexpr = TConst(TFloat "0.0") })
                | TEnum({ e_path = ([], "Bool") }, []) -> write w "false"
                | _ -> write w "null")
            | TThis -> write w "this"
            | TSuper -> write w "super")
        | TLocal { v_name = "__fallback__" } -> ()
        | TLocal { v_name = "__sbreak__" } -> write w "break"
        | TLocal { v_name = "__undefined__" } ->
          write w (t_s (TInst(runtime_cl, List.map (fun _ -> t_dynamic) runtime_cl.cl_types)));
          write w ".undefined";
        | TLocal var ->
          write_id w var.v_name
        | TEnumField (e, s) ->
          print w "%s." (path_s e.e_path); write_field w s
        | TArray (e1, e2) ->
          expr_s w e1; write w "["; expr_s w e2; write w "]"
        | TBinop ((Ast.OpAssign as op), e1, e2)
        | TBinop ((Ast.OpAssignOp _ as op), e1, e2) ->
          expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2
        | TBinop (op, e1, e2) ->
          write w "( ";
          expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2;
          write w " )"
        | TField (e, s) | TClosure (e, s) ->
          expr_s w e; write w "."; write_field w s
        | TTypeExpr (TClassDecl { cl_path = (["haxe"], "Int32") }) ->
          write w (path_s (["haxe"], "Int32"))
        | TTypeExpr (TClassDecl { cl_path = (["haxe"], "Int64") }) ->
          write w (path_s (["haxe"], "Int64"))
        | TTypeExpr mt -> write w (md_s mt)
        | TParenthesis e ->
          write w "("; expr_s w e; write w ")"
        | TArrayDecl el when t_has_type_param_shallow false e.etype ->
          print w "( (%s) (new java.lang.Object[] " (t_s e.etype);
          write w "{";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            acc + 1
          ) 0 el);
          write w "}) )"
        | TArrayDecl el ->
          print w "new %s" (param_t_s (transform_nativearray_t e.etype));
          let is_double = match follow e.etype with
           | TInst(_,[ t ]) -> ( match follow t with | TInst({ cl_path=([],"Float") },[]) -> Some t | _ -> None )
           | _ -> None
          in

          write w "{";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            (* this is a hack so we are able to convert ints to boxed Double / Float when needed *)
            let e = if is_some is_double then mk_cast (get is_double) e else e in

            expr_s w e;
            acc + 1
          ) 0 el);
          write w "}"
        | TCall( ( { eexpr = TField({ eexpr = TTypeExpr (TClassDecl { cl_path = ([], "String") }) }, "fromCharCode") } ), [cc] ) ->
            write w "Character.toString((char) ";
            expr_s w cc;
            write w ")"
        | TCall ({ eexpr = TLocal( { v_name = "__is__" } ) }, [ expr; { eexpr = TTypeExpr(md) } ] ) ->
          write w "( ";
          expr_s w expr;
          write w " instanceof ";
          write w (md_s md);
          write w " )"
        | TCall ({ eexpr = TLocal( { v_name = "__java__" } ) }, [ { eexpr = TConst(TString(s)) } ] ) ->
          write w s
        | TCall ({ eexpr = TLocal( { v_name = "__lock__" } ) }, [ eobj; eblock ] ) ->
          write w "synchronized(";
          expr_s w eobj;
          write w ")";
          expr_s w (mk_block eblock)
        | TCall ({ eexpr = TLocal( { v_name = "__goto__" } ) }, [ { eexpr = TConst(TInt v) } ] ) ->
          print w "break label%ld" v
        | TCall ({ eexpr = TLocal( { v_name = "__label__" } ) }, [ { eexpr = TConst(TInt v) } ] ) ->
          print w "label%ld:" v
        | TCall ({ eexpr = TLocal( { v_name = "__typeof__" } ) }, [ { eexpr = TTypeExpr md } as expr ] ) ->
          expr_s w expr;
          write w ".class"
        | TCall (e, el) ->
          let rec extract_tparams params el =
            match el with
              | ({ eexpr = TLocal({ v_name = "$type_param" }) } as tp) :: tl ->
                extract_tparams (tp.etype :: params) tl
              | _ -> (params, el)
          in
          let params, el = extract_tparams [] el in

          expr_s w e;

          (*(match params with
            | [] -> ()
            | params ->
              let md = match e.eexpr with
                | TField(ef, _) -> t_to_md (run_follow gen ef.etype)
                | _ -> assert false
              in
              write w "<";
              ignore (List.fold_left (fun acc t ->
                (if acc <> 0 then write w ", ");
                write w (param_t_s (change_param_type md t));
                acc + 1
              ) 0 params);
              write w ">"
          );*)

          write w "(";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            acc + 1
          ) 0 el);
          write w ")"
        | TNew (({ cl_path = (["java"], "NativeArray") } as cl), params, [ size ]) ->
          let rec check_t_s t times =
            match real_type t with
              | TInst({ cl_path = (["java"], "NativeArray") }, [param]) ->
                (check_t_s param (times+1))
              | _ ->
                print w "new %s[" (t_s (transform_nativearray_t t));
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
            (if acc <> 0 then write w "; ");
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
          (*let last_line = ref (-1) in
          let line_directive p =
            let cur_line = Lexer.get_error_line p in
            let is_relative_path = (String.sub p.pfile 0 1) = "." in
            let file = if is_relative_path then "../" ^ p.pfile else p.pfile in
            if cur_line <> ((!last_line)+1) then begin print w "//#line %d \"%s\"" cur_line (Ast.s_escape file); newline w end;
            last_line := cur_line in*)
          List.iter (fun e ->
            (*line_directive e.epos;*)
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
        | TMatch _ -> write w "[ match not supported ]"; if !strict_mode then assert false
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

  let gen_class_field w is_static cl is_final cf =
    let is_interface = cl.cl_interface in
    let name, is_new, is_explicit_iface = match cf.cf_name with
      | "new" -> snd cl.cl_path, true, false
      | name when String.contains name '.' ->
        let fn_name, path = parse_explicit_iface name in
        (path_s path) ^ "." ^ fn_name, false, true
      | name -> name, false, false
    in
    (match cf.cf_kind with
      | Var _
      | Method (MethDynamic) ->
        if not is_interface then begin
          let access, modifiers = get_fun_modifiers cf.cf_meta "public" [] in
          print w "%s %s%s %s %s;" access (if is_static then "static " else "") (String.concat " " modifiers) (t_s (run_follow gen cf.cf_type)) (change_field name)
        end (* TODO see how (get,set) variable handle when they are interfaces *)
      | Method mkind ->
        let is_virtual = is_new || (not is_final && match mkind with | MethInline -> false | _ when not is_new -> true | _ -> false) in
        let is_override = match cf.cf_name with
          | "equals" when not is_static ->
            (match cf.cf_type with
              | TFun([_,_,t], ret) ->
                (match (real_type t, real_type ret) with
                  | TDynamic _, TEnum( { e_path = ([], "Bool") }, [])
                  | TAnon _, TEnum( { e_path = ([], "Bool") }, []) -> true
                  | _ -> List.mem cf.cf_name cl.cl_overrides
                )
              | _ -> List.mem cf.cf_name cl.cl_overrides)
          | "toString" when not is_static ->
            (match cf.cf_type with
              | TFun([], ret) ->
                (match real_type ret with
                  | TInst( { cl_path = ([], "String") }, []) -> true
                  | _ -> gen.gcon.error "A toString() function should return a String!" cf.cf_pos; false
                )
              | _ -> List.mem cf.cf_name cl.cl_overrides
            )
          | "hashCode" when not is_static ->
            (match cf.cf_type with
              | TFun([], ret) ->
                (match real_type ret with
                  | TInst( { cl_path = ([], "Int") }, []) ->
                    true
                  | _ -> gen.gcon.error "A hashCode() function should return an Int!" cf.cf_pos; false
                )
              | _ -> List.mem cf.cf_name cl.cl_overrides
            )
          | _ -> List.mem cf.cf_name cl.cl_overrides
        in
        let visibility = if is_interface then "" else "public" in

        let visibility, modifiers = get_fun_modifiers cf.cf_meta visibility [] in
        let visibility, is_virtual = if is_explicit_iface then "",false else visibility, is_virtual in
        let v_n = if is_static then "static " else if is_override && not is_interface then "" else if not is_virtual then "final " else "" in
        let cf_type = if is_override then match field_access gen (TInst(cl, List.map snd cl.cl_types)) cf.cf_name with | FClassField(_,_,_,_,actual_t) -> actual_t | _ -> assert false else cf.cf_type in

        let params = List.map snd cl.cl_types in
        let ret_type, args = match cf_type, cf.cf_type with | TFun (strbtl, t), TFun(rargs, _) -> (apply_params cl.cl_types params (real_type t), List.map2 (fun(_,_,t) (n,o,_) -> (n,o,apply_params cl.cl_types params (real_type t))) strbtl rargs) | _ -> assert false in

        (if is_override && not is_interface then write w "@Override ");
        (* public static void funcName *)
        let params, _ = get_string_params cf.cf_params in
        print w "%s %s%s %s %s %s" (visibility) v_n (String.concat " " modifiers) params (if is_new then "" else rett_s (run_follow gen ret_type)) (change_field name);

        (* <T>(string arg1, object arg2) with T : object *)
        print w "(%s)" (String.concat ", " (List.map (fun (name, _, t) -> sprintf "%s %s" (t_s (run_follow gen t)) (change_id name)) args));
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
                  expr_s w expr
                end else begin
                  expr_s w expr;
                end)
              | (":throws", [Ast.EConst (Ast.String t), _], _) :: tl ->
                print w " throws %s" t;
                loop tl
              | (":functionBody", [Ast.EConst (Ast.String contents),_],_) :: tl ->
                begin_block w;
                write w contents;
                end_block w
              | _ :: tl -> loop tl
          in
          loop cf.cf_meta

        end);
      newline w;
      newline w
  in

  let gen_class w cl =
    let should_close = match change_ns (fst cl.cl_path) with
      | [] -> false
      | ns ->
        print w "package %s;" (String.concat "." (change_ns ns));
        newline w;
        false
    in

    let rec loop_meta meta acc =
      match meta with
        | (":SuppressWarnings", [Ast.EConst (Ast.String w),_],_) :: meta -> loop_meta meta (w :: acc)
        | _ :: meta -> loop_meta meta acc
        | _ -> acc
    in

    let suppress_warnings = loop_meta cl.cl_meta [ "rawtypes"; "unchecked" ] in

    write w "import haxe.root.*;";
    newline w;
    write w "@SuppressWarnings(value={";
    let first = ref true in
    List.iter (fun s ->
      (if !first then first := false else write w ", ");
      print w "\"%s\"" (escape s)
    ) suppress_warnings;
    write w "})";
    newline w;

    let clt, access, modifiers = get_class_modifiers cl.cl_meta (if cl.cl_interface then "interface" else "class") "public" [] in
    let is_final = has_meta ":final" cl.cl_meta in

    print w "%s %s %s %s" access (String.concat " " modifiers) clt (change_clname (snd cl.cl_path));
    (* type parameters *)
    let params, _ = get_string_params cl.cl_types in
    let cl_p_to_string (cl,p) = path_param_s (TClassDecl cl) cl.cl_path p in
    print w "%s" params;
    (if is_some cl.cl_super then print w " extends %s" (cl_p_to_string (get cl.cl_super)));
    (match cl.cl_implements with
      | [] -> ()
      | _ -> print w " %s %s" (if cl.cl_interface then "extends" else "implements") (String.concat ", " (List.map cl_p_to_string cl.cl_implements))
    );
    (* class head ok: *)
    (* public class Test<A> : X, Y, Z where A : Y *)
    begin_block w;
    (* our constructor is expected to be a normal "new" function *
    if !strict_mode && is_some cl.cl_constructor then assert false;*)

    let rec loop meta =
      match meta with
        | [] ->  ()
        | (":classContents", [Ast.EConst (Ast.String contents),_],_) :: tl ->
          write w contents
        | _ :: tl -> loop tl
    in
    loop cl.cl_meta;

    (match gen.gcon.main_class with
      | Some path when path = cl.cl_path ->
        write w "public static void main(String[] args)";
        begin_block w;
        (try
          let t = Hashtbl.find gen.gtypes ([], "Sys") in
              match t with 
                | TClassDecl(cl) when PMap.mem "_args" cl.cl_statics -> 
                  write w "Sys._args = args;"; newline w
                | _ -> ()
        with | Not_found -> ()
        );
        write w "main();";
        end_block w
      | _ -> ()
    );

    (match cl.cl_init with
      | None -> ()
      | Some init ->
        write w "static ";
        expr_s w (mk_block init));
    (if is_some cl.cl_constructor then gen_class_field w false cl is_final (get cl.cl_constructor));
    (if not cl.cl_interface then
      List.iter (gen_class_field w true cl is_final) cl.cl_ordered_statics);
    List.iter (gen_class_field w false cl is_final) cl.cl_ordered_fields;
    end_block w;
    if should_close then end_block w
  in


  let gen_enum w e =
    let should_close = match change_ns (fst e.e_path) with
      | [] -> false
      | ns ->
        print w "package %s;" (String.concat "." (change_ns ns));
        newline w;
        false
    in

    print w "public enum %s" (change_clname (snd e.e_path));
    begin_block w;
    write w (String.concat ", " e.e_names);
    end_block w;

    if should_close then end_block w
  in

  let module_type_gen w md_tp =
    match md_tp with
      | TClassDecl cl ->
        if not cl.cl_extern then begin
          gen_class w cl;
          newline w;
          newline w
        end;
        (not cl.cl_extern)
      | TEnumDecl e ->
        if not e.e_extern then begin
          gen_enum w e;
          newline w;
          newline w
        end;
        (not e.e_extern)
      | TTypeDecl e ->
        false
  in

  let module_gen w md =
    module_type_gen w md
  in

  (* generate source code *)
  init_ctx gen;

  Hashtbl.add gen.gspecial_vars "__label__" true;
  Hashtbl.add gen.gspecial_vars "__goto__" true;
  Hashtbl.add gen.gspecial_vars "__is__" true;
  Hashtbl.add gen.gspecial_vars "__typeof__" true;
  Hashtbl.add gen.gspecial_vars "__java__" true;
  Hashtbl.add gen.gspecial_vars "__lock__" true;

  gen.greal_type <- real_type;
  gen.greal_type_param <- change_param_type;

  SetHXGen.run_filter gen SetHXGen.default_hxgen_func;

  let closure_t = ClosuresToClass.DoubleAndDynamicClosureImpl.get_ctx gen 6 in

  (*let closure_t = ClosuresToClass.create gen 10 float_cl
    (fun l -> l)
    (fun l -> l)
    (fun args -> args)
    (fun args -> [])
  in
  ClosuresToClass.configure gen (ClosuresToClass.default_implementation closure_t (fun e _ _ -> e));

  StubClosureImpl.configure gen (StubClosureImpl.default_implementation gen float_cl 10 (fun e _ _ -> e));*)

  IteratorsInterface.configure gen (fun e -> e);

  ClosuresToClass.configure gen (ClosuresToClass.default_implementation closure_t (get_cl (get_type gen (["haxe";"lang"],"Function")) ));

  EnumToClass.configure gen (None) false true (get_cl (get_type gen (["haxe";"lang"],"Enum")) ) false true;

  InterfaceVarsDeleteModf.configure gen;

  let dynamic_object = (get_cl (get_type gen (["haxe";"lang"],"DynamicObject")) ) in

  let object_iface = get_cl (get_type gen (["haxe";"lang"],"IHxObject")) in

  (*fixme: THIS IS A HACK. take this off *)
  let empty_e = match (get_type gen (["haxe";"lang"], "EmptyObject")) with | TEnumDecl e -> e | _ -> assert false in
  (*OverloadingCtor.set_new_create_empty gen ({eexpr=TEnumField(empty_e, "EMPTY"); etype=TEnum(empty_e,[]); epos=null_pos;});*)

  OverloadingConstructor.configure gen (TEnum(empty_e, [])) ({eexpr=TEnumField(empty_e, "EMPTY"); etype=TEnum(empty_e,[]); epos=null_pos;}) false;

  let rcf_static_find = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "findHash" Ast.null_pos [] in
  (*let rcf_static_lookup = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "lookupHash" Ast.null_pos [] in*)

  let can_be_float t = match follow (real_type t) with
    | TInst({ cl_path = (["haxe"], "Int32")}, [] )
    | TInst({ cl_path = ([], "Int") }, [])
    | TInst({ cl_path = ([], "Float") }, []) -> true
    | _ -> false
  in

  let rcf_on_getset_field main_expr field_expr field may_hash may_set is_unsafe =
    let is_float = can_be_float (if is_none may_set then main_expr.etype else (get may_set).etype) in
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

    mk_cast ecall.etype { ecall with eexpr = TCall(infer, call_args); etype = t_dynamic }
  in

  let rcf_ctx = ReflectionCFs.new_ctx gen closure_t object_iface false rcf_on_getset_field rcf_on_call_field (fun hash hash_array ->
    { hash with eexpr = TCall(rcf_static_find, [hash; hash_array]); etype=basic.tint }
  ) (fun hash -> hash ) false in

  ReflectionCFs.UniversalBaseClass.default_config gen (get_cl (get_type gen (["haxe";"lang"],"HxObject")) ) object_iface dynamic_object;

  ReflectionCFs.configure_dynamic_field_access rcf_ctx false;

  (* let closure_func = ReflectionCFs.implement_closure_cl rcf_ctx ( get_cl (get_type gen (["haxe";"lang"],"Closure")) ) in *)
  let closure_cl = get_cl (get_type gen (["haxe";"lang"],"Closure")) in

  let closure_func = ReflectionCFs.get_closure_func rcf_ctx closure_cl in

  ReflectionCFs.implement_varargs_cl rcf_ctx ( get_cl (get_type gen (["haxe";"lang"], "VarArgsBase")) );

  let slow_invoke = mk_static_field_access_infer (runtime_cl) "slowCallField" Ast.null_pos [] in
  ReflectionCFs.configure rcf_ctx ~slow_invoke:(fun ethis efield eargs -> {
    eexpr = TCall(slow_invoke, [ethis; efield; eargs]);
    etype = t_dynamic;
    epos = ethis.epos;
  } );

  let objdecl_fn = ReflectionCFs.implement_dynamic_object_ctor rcf_ctx dynamic_object in

  ObjectDeclMap.configure gen (ObjectDeclMap.traverse gen objdecl_fn);

  InitFunction.configure gen true;
  TArrayTransform.configure gen (TArrayTransform.default_implementation gen (
  fun e ->
    match e.eexpr with
      | TArray(e1, e2) ->
        ( match follow e1.etype with
          | TInst({ cl_path = (["java"], "NativeArray") }, _) -> false
          | _ -> true )
      | _ -> assert false
  ) "__get" "__set" );

  let field_is_dynamic t field =
    match field_access gen (gen.greal_type t) field with
      | FClassField (cl,p,_,_,t) ->
        is_dynamic (apply_params cl.cl_types p t)
      | FEnumField _ -> false
      | _ -> true
  in

  let is_type_param e = match follow e with
    | TInst( { cl_kind = KTypeParameter _ },[]) -> true
    | _ -> false
  in

  let is_dynamic_expr e = is_dynamic e.etype || match e.eexpr with
    | TField(tf, f) -> field_is_dynamic tf.etype f
    | _ -> false
  in

  let may_nullable t = match gen.gfollow#run_f t with
    | TType({ t_path = ([], "Null") }, [t]) ->
      (match follow t with
        | TInst({ cl_path = ([], "String") }, [])
        | TInst({ cl_path = ([], "Float") }, [])
        | TInst({ cl_path = (["haxe"], "Int32")}, [] )
        | TInst({ cl_path = (["haxe"], "Int64")}, [] )
        | TInst({ cl_path = ([], "Int") }, [])
        | TEnum({ e_path = ([], "Bool") }, []) -> Some t
        | _ -> None )
    | _ -> None
  in

  let is_double t = match follow t with | TInst({ cl_path = ([], "Float") }, []) -> true | _ -> false in
  let is_int t = match follow t with | TInst({ cl_path = ([], "Int") }, []) -> true | _ -> false in

  DynamicOperators.configure gen
    (DynamicOperators.abstract_implementation gen (fun e -> match e.eexpr with
      | TBinop (Ast.OpEq, e1, e2)
      | TBinop (Ast.OpAdd, e1, e2)
      | TBinop (Ast.OpNotEq, e1, e2) -> is_dynamic e1.etype or is_dynamic e2.etype or is_type_param e1.etype or is_type_param e2.etype
      | TBinop (Ast.OpLt, e1, e2)
      | TBinop (Ast.OpLte, e1, e2)
      | TBinop (Ast.OpGte, e1, e2)
      | TBinop (Ast.OpGt, e1, e2) -> is_dynamic e.etype or is_dynamic_expr e1 or is_dynamic_expr e2 or is_string e1.etype or is_string e2.etype
      | TBinop (_, e1, e2) -> is_dynamic e.etype or is_dynamic_expr e1 or is_dynamic_expr e2
      | TUnop (_, _, e1) -> is_dynamic_expr e1
      | _ -> false)
    (fun e1 e2 ->
      let is_null e = match e.eexpr with | TConst(TNull) | TLocal({ v_name = "__undefined__" }) -> true | _ -> false in

      if is_null e1 || is_null e2 then
        match e1.eexpr, e2.eexpr with
          | TConst c1, TConst c2 ->
            { e1 with eexpr = TConst(TBool (c1 = c2)); etype = basic.tbool }
          | _ ->
            { e1 with eexpr = TBinop(Ast.OpEq, e1, e2); etype = basic.tbool }
      else begin
        let is_ref = match follow e1.etype, follow e2.etype with
          | TDynamic _, _
          | _, TDynamic _
          | TInst({ cl_path = ([], "Float") },[]), _
          | TInst( { cl_path = (["haxe"], "Int32") }, [] ), _
          | TInst( { cl_path = (["haxe"], "Int64") }, [] ), _
          | TInst({ cl_path = ([], "Int") },[]), _
          | TEnum({ e_path = ([], "Bool") },[]), _
          | _, TInst({ cl_path = ([], "Float") },[])
          | _, TInst({ cl_path = ([], "Int") },[])
          | _, TInst( { cl_path = (["haxe"], "Int32") }, [] )
          | _, TInst( { cl_path = (["haxe"], "Int64") }, [] )
          | _, TEnum({ e_path = ([], "Bool") },[])
          | TInst( { cl_kind = KTypeParameter _ }, [] ), _
          | _, TInst( { cl_kind = KTypeParameter _ }, [] ) -> false
          | _, _ -> true
        in

        let static = mk_static_field_access_infer (runtime_cl) (if is_ref then "refEq" else "eq") e1.epos [] in
        { eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tbool; epos=e1.epos }
      end
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
        | _ ->
          let static = mk_static_field_access_infer (runtime_cl) "plus"  e1.epos [] in
          mk_cast e.etype { eexpr = TCall(static, [e1; e2]); etype = t_dynamic; epos=e1.epos })
    (fun e1 e2 ->
      if is_string e1.etype then begin
        { e1 with eexpr = TCall({ e1 with eexpr = TField(e1, "compareTo"); etype = TFun(["anotherString",false,gen.gcon.basic.tstring], gen.gcon.basic.tint) }, [ e2 ]); etype = gen.gcon.basic.tint }
      end else begin
        let static = mk_static_field_access_infer (runtime_cl) "compare" e1.epos [] in
        { eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tint; epos=e1.epos }
      end));

  FilterClosures.configure gen (FilterClosures.traverse gen (fun e1 s -> true) closure_func);

  let base_exception = get_cl (get_type gen (["java"; "lang"], "Throwable")) in
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
        { throwexpr with eexpr = TThrow { expr with eexpr = TCall(wrap_static, [expr]) }; etype = gen.gcon.basic.tvoid }
      )
      (fun v_to_unwrap pos ->
        let local = mk_cast hx_exception_t { eexpr = TLocal(v_to_unwrap); etype = v_to_unwrap.v_type; epos = pos } in
        { eexpr = TField(local, "obj"); epos = pos; etype = t_dynamic }
      )
      (fun rethrow ->
        let wrap_static = mk_static_field_access (hx_exception) "wrap" (TFun([("obj",false,t_dynamic)], base_exception_t)) rethrow.epos in
        { rethrow with eexpr = TThrow { rethrow with eexpr = TCall(wrap_static, [rethrow]) }; }
      )
      (base_exception_t)
      (hx_exception_t)
      (fun v e -> e)
  );

  let get_typeof e =
    { e with eexpr = TCall( { eexpr = TLocal( alloc_var "__typeof__" t_dynamic ); etype = t_dynamic; epos = e.epos }, [e] ) }
  in

  ClassInstance.configure gen (ClassInstance.traverse gen (fun e mt -> get_typeof e));

  (*let v = alloc_var "$type_param" t_dynamic in*)
  TypeParams.configure gen (fun ecall efield params elist ->
    { ecall with eexpr = TCall(efield, elist) }
  );

  CastDetect.configure gen (CastDetect.default_implementation gen (Some (TEnum(empty_e, []))) true);

  (*FollowAll.configure gen;*)

  SwitchToIf.configure gen (SwitchToIf.traverse gen (fun e ->
    match e.eexpr with
      | TSwitch(cond, cases, def) ->
        (match gen.gfollow#run_f cond.etype with
          | TInst( { cl_path = (["haxe"], "Int32") }, [] )
          | TInst({ cl_path = ([], "Int") },[])
          | TInst({ cl_path = ([], "String") },[]) ->
            (List.exists (fun (c,_) ->
              List.exists (fun expr -> match expr.eexpr with | TConst _ -> false | _ -> true ) c
            ) cases)
          | _ -> true
        )
      | _ -> assert false
  ) true );

  let native_arr_cl = get_cl ( get_type gen (["java"], "NativeArray") ) in

  ExpressionUnwrap.configure gen (ExpressionUnwrap.traverse gen (fun e -> Some { eexpr = TVars([mk_temp gen "expr" e.etype, Some e]); etype = gen.gcon.basic.tvoid; epos = e.epos }));

  UnnecessaryCastsRemoval.configure gen;

  IntDivisionSynf.configure gen (IntDivisionSynf.default_implementation gen true);

  UnreachableCodeEliminationSynf.configure gen (UnreachableCodeEliminationSynf.traverse gen true true true true);

  ArrayDeclSynf.configure gen (ArrayDeclSynf.default_implementation gen native_arr_cl);

  let goto_special = alloc_var "__goto__" t_dynamic in
  let label_special = alloc_var "__label__" t_dynamic in
  SwitchBreakSynf.configure gen (SwitchBreakSynf.traverse gen
    (fun e_loop n api ->
      { e_loop with eexpr = TBlock( { eexpr = TCall( mk_local label_special e_loop.epos, [ mk_int gen n e_loop.epos ] ); etype = t_dynamic; epos = e_loop.epos } :: [e_loop] ) };
    )
    (fun e_break n api ->
      { eexpr = TCall( mk_local goto_special e_break.epos, [ mk_int gen n e_break.epos ] ); etype = t_dynamic; epos = e_break.epos }
    )
  );

  DefaultArguments.configure gen (DefaultArguments.traverse gen);

  JavaSpecificSynf.configure gen (JavaSpecificSynf.traverse gen runtime_cl);
  JavaSpecificESynf.configure gen (JavaSpecificESynf.traverse gen runtime_cl);

  (* add native String as a String superclass *)
  let str_cl = match gen.gcon.basic.tstring with | TInst(cl,_) -> cl | _ -> assert false in
  str_cl.cl_super <- Some (get_cl (get_type gen (["haxe";"lang"], "NativeString")), []);

  let mkdir dir = if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 in
  mkdir gen.gcon.file;
  mkdir (gen.gcon.file ^ "/src");

  (* add resources array *)
  (try
    let res = get_cl (Hashtbl.find gen.gtypes (["haxe"], "Resource")) in
    let cf = PMap.find "content" res.cl_statics in
    let res = ref [] in
    Hashtbl.iter (fun name v ->
      res := { eexpr = TConst(TString name); etype = gen.gcon.basic.tstring; epos = Ast.null_pos } :: !res;
      let f = open_out (gen.gcon.file ^ "/src/" ^ name) in
      output_string f v;
      close_out f
    ) gen.gcon.resources;
    cf.cf_expr <- Some ({ eexpr = TArrayDecl(!res); etype = gen.gcon.basic.tarray gen.gcon.basic.tstring; epos = Ast.null_pos })
  with | Not_found -> ());

  run_filters gen;

  TypeParams.RenameTypeParameters.run gen;

  let t = Common.timer "code generation" in

	generate_modules_t gen "java" "src" change_path module_gen;

  dump_descriptor gen ("hxjava_build.txt") path_s;
	if ( not (Common.defined gen.gcon "no-compilation") ) then begin
		let old_dir = Sys.getcwd() in
		Sys.chdir gen.gcon.file;
		let cmd = "haxelib run hxjava hxjava_build.txt --haxe-version " ^ (string_of_int gen.gcon.version) in
		print_endline cmd;
		if Sys.command cmd <> 0 then failwith "Build failed";
		Sys.chdir old_dir;
	end;

  t()

(* end of configure function *)
	
let before_generate con =
  ()

let generate con =
  let gen = new_ctx con in
  gen.gallow_tp_dynamic_conversion <- true;

  let basic = con.basic in
  (* make the basic functions in java *)
  let basic_fns =
  [
    mk_class_field "equals" (TFun(["obj",false,t_dynamic], basic.tbool)) true Ast.null_pos (Method MethNormal) [];
    mk_class_field "toString" (TFun([], basic.tstring)) true Ast.null_pos (Method MethNormal) [];
    mk_class_field "hashCode" (TFun([], basic.tint)) true Ast.null_pos (Method MethNormal) [];
  ] in
  List.iter (fun cf -> gen.gbase_class_fields <- PMap.add cf.cf_name cf gen.gbase_class_fields) basic_fns;

  (try
    configure gen
  with | TypeNotFound path -> con.error ("Error. Module '" ^ (path_s path) ^ "' is required and was not included in build.")  Ast.null_pos);
  debug_mode := false

(** Java lib *)

open JData

exception ConversionError of string * pos

let error s p = raise (ConversionError (s, p))

let mk_type_path path params =
  CTPath {
    tpackage = fst path;
    tname = snd (String.replace ~str:(snd path) ~sub:"$" ~by:"_"); (* handle with inner classes *)
    tparams = params;
    tsub = None;
  }

let rec convert_arg p arg =
  match arg with
  | TAny | TType (WSuper, _) -> TPType (mk_type_path ([], "Dynamic") [])
  | TType (_, jsig) -> TPType (convert_signature p jsig)

and convert_signature p jsig =
  match jsig with
  | TByte -> mk_type_path (["java"; "StdTypes"], "Int8") []
  | TChar -> mk_type_path (["java"; "StdTypes"], "Char16") []
  | TDouble -> mk_type_path ([], "Float") []
  | TFloat -> mk_type_path ([], "Single") []
  | TInt -> mk_type_path ([], "Int") []
  | TLong -> mk_type_path (["haxe"], "Int64") []
  | TShort -> mk_type_path (["java"; "StdTypes"], "Int16") []
  | TBool -> mk_type_path ([], "Bool") []
  | TObject ( (["haxe";"root"], name), args ) -> mk_type_path ([], name) (List.map (convert_arg p) args)
  (** nullable types *)
  | TObject ( (["java";"lang"], "Integer"), [] ) -> mk_type_path ([], "Null") [ TPType (mk_type_path ([], "Int") []) ]
  | TObject ( (["java";"lang"], "Double"), [] ) -> mk_type_path ([], "Null") [ TPType (mk_type_path ([], "Float") []) ]
  | TObject ( (["java";"lang"], "Single"), [] ) -> mk_type_path ([], "Null") [ TPType (mk_type_path ([], "Single") []) ]
  | TObject ( (["java";"lang"], "Boolean"), [] ) -> mk_type_path ([], "Null") [ TPType (mk_type_path ([], "Bool") []) ]
  | TObject ( (["java";"lang"], "Byte"), [] ) -> mk_type_path ([], "Null") [ TPType (mk_type_path (["java";"StdTypes"], "Int8") []) ]
  | TObject ( (["java";"lang"], "Character"), [] ) -> mk_type_path ([], "Null") [ TPType (mk_type_path (["java";"StdTypes"], "Char16") []) ]
  | TObject ( (["java";"lang"], "Short"), [] ) -> mk_type_path ([], "Null") [ TPType (mk_type_path (["java";"StdTypes"], "Int16") []) ]
  | TObject ( (["java";"lang"], "Long"), [] ) -> mk_type_path ([], "Null") [ TPType (mk_type_path (["haxe"], "Int64") []) ]
  (** other std types *)
  | TObject ( (["java";"lang"], "Object"), [] ) -> mk_type_path ([], "Dynamic") []
  | TObject ( (["java";"lang"], "String"), [] ) -> mk_type_path ([], "String") []
  | TObject ( (["java";"lang"], "Class"), args ) -> mk_type_path ([], "Class") (List.map (convert_arg p) args)
  (** other types *)
  | TObject ( path, args ) -> mk_type_path path (List.map (convert_arg p) args)
  | TArray (jsig, _) -> mk_type_path (["java"], "NativeArray") [ TPType (convert_signature p jsig) ]
  | TMethod _ -> JReader.error "TMethod cannot be converted directly into Complex Type"
  | TTypeParameter s -> mk_type_path ([], s) []

let convert_constant p const =
  Option.map_default (function
    | ConstString s -> Some (EConst (String s), p)
    | ConstInt i -> Some (EConst (Int (Printf.sprintf "%ld" i)), p)
    | ConstFloat f | ConstDouble f -> Some (EConst (Float (Printf.sprintf "%E" f)), p)
    | _ -> None) None const

let convert_param p param =
  let name, constraints = match param with
    | (name, Some extends_sig, implem_sig) ->
      name, extends_sig :: implem_sig
    | (name, None, implemem_sig) ->
      name, implemem_sig
    in
    {
      tp_name = name;
      tp_params = [];
      tp_constraints = List.map (convert_signature p) constraints;
    }


let get_type_path ct = match ct with | CTPath p -> p | _ -> assert false

let convert_java_enum p pe =
  let meta = ref [":$javaNative", [], p] in
  let data = ref [] in
  List.iter (fun f ->
    if List.mem JEnum f.jf_flags then
      data := (f.jf_name, None, [], [], p) :: !data
  ) pe.cfields;

  EEnum {
    d_name = snd pe.cpath;
    d_doc = None;
    d_params = []; (* enums never have type parameters *)
    d_meta = !meta;
    d_flags = [EExtern];
    d_data = !data;
  }

let convert_java_field p jc field =
  let cff_doc = None in
  let cff_pos = p in
  let cff_meta = ref [] in
  let cff_access = ref [] in
  let cff_name = match field.jf_name with
    | "<init>" -> "new"
    | "<clinit>" -> cff_access := [AStatic]; "__init__"
    | name -> name
  in

  List.iter (function
    | JPublic -> cff_access := APublic :: !cff_access
    | JPrivate -> raise Exit (* private instances aren't useful on externs *)
    | JProtected -> cff_access := APrivate :: !cff_access
    | JStatic -> cff_access := AStatic :: !cff_access
    | JFinal -> cff_meta := (":final", [], p) :: !cff_meta
    | JSynchronized -> cff_meta := (":synchronized", [], p) :: !cff_meta
    | JVolatile -> cff_meta := (":volatile", [], p) :: !cff_meta
    | JTransient -> cff_meta := (":transient", [], p) :: !cff_meta
    | JVarArgs -> cff_meta := (":varArgs", [], p) :: !cff_meta
    | _ -> ()
  ) field.jf_flags;

  List.iter (function
    | AttrDeprecated -> cff_meta := (":deprecated", [], p) :: !cff_meta
    (* TODO: pass anotations as @:meta *)
    | AttrVisibleAnnotations ann ->
      List.iter (function 
        | { ann_type = TObject( (["java";"lang"], "Override"), [] ) } ->
          cff_access := AOverride :: !cff_access
        | _ -> ()
      ) ann
    | _ -> ()
  ) field.jf_attributes;

  let kind = match field.jf_kind with
    | JKField -> 
      FVar (Some (convert_signature p field.jf_signature), convert_constant p field.jf_constant)
    | JKMethod ->
      match field.jf_signature with
      | TMethod (args, ret) ->
        let i = ref 0 in
        let args = List.map (fun s ->
          incr i;
          "param" ^ string_of_int !i, false, Some(convert_signature p s), None
        ) args in
        let t = Option.map_default (convert_signature p) (mk_type_path ([], "Void") []) ret in
        cff_meta := (":overload", [], p) :: !cff_meta;

        let types = List.map (function
          | (name, Some ext, impl) ->
            {
              tp_name = name;
              tp_params = [];
              tp_constraints = List.map (convert_signature p) (ext :: impl);
            }
          | (name, None, impl) ->
            {
              tp_name = name;
              tp_params = [];
              tp_constraints = List.map (convert_signature p) (impl);
            }
        ) field.jf_types in
        
        FFun ({
          f_params = types;
          f_args = args;
          f_type = Some t;
          f_expr = None
        })
      | _ -> error "Method signature was expected" p
  in

  {
    cff_name = cff_name;
    cff_doc = cff_doc;
    cff_pos = cff_pos;
    cff_meta = !cff_meta;
    cff_access = !cff_access;
    cff_kind = kind
  }


let convert_java_class p jc =
  match List.mem JEnum jc.cflags with
  | true -> (* is enum *)
    convert_java_enum p jc
  | false ->
    let flags = ref [HExtern] in
    let meta = ref [":$javaNative", [], p] in

    List.iter (fun f -> match f with
      | JFinal -> meta := (":final", [], p) :: !meta
      | JInterface -> flags := HInterface :: !flags
      | JAbstract -> meta := (":abstract", [], p) :: !meta
      | JAnnotation -> meta := (":annotation", [], p) :: !meta
      | _ -> ()
    ) jc.cflags;
    
    (match jc.csuper with
      | TObject( (["java";"lang"], "Object"), _ ) -> ()
      | TObject( (["haxe";"lang"], "HxObject"), _ ) -> meta := (":hxgen",[],p) :: !meta
      | _ -> flags := HExtends (get_type_path (convert_signature p jc.csuper)) :: !flags
    );

    List.iter (fun i ->
      match i with
      | TObject ( (["haxe";"lang"], "IHxObject"), _ ) -> meta := (":hxgen",[],p) :: !meta
      | _ -> flags := HImplements (get_type_path (convert_signature p i)) :: !flags
    ) jc.cinterfaces;

    EClass {
      d_name = snd jc.cpath;
      d_doc = None;
      d_params = List.map (convert_param p) jc.ctypes;
      d_meta = !meta;
      d_flags = !flags;
      d_data = List.map (convert_java_field p jc) jc.cfields @ List.map (convert_java_field p jc) jc.cmethods;
    }


let add_java_lib com file =
  let get_raw_class, close = 
    let file = if Sys.file_exists file then
      file
    else if Sys.file_exists (file ^ ".jar") then
      file ^ ".jar"
    else
      failwith "Java lib " ^ file ^ " not found"
    in

    (* check if it is a directory or jar file *)
    match Sys.is_directory file with
    | true -> (* open classes directly from directory *)
      (fun (pack, name) ->
        let real_path = file ^ "/" ^ (String.concat "." pack) ^ "/" ^ name ^ ".class" in
        try
          let data = Std.input_file ~bin:true real_path in
          Some (IO.input_string data), real_path, real_path
        with 
          | _ -> None, real_path, real_path), (fun () -> ())
    | false -> (* open zip file *)
      let zip = Zip.open_in file in
      let closed = ref false in
      (fun (pack, name) -> 
        if !closed then failwith "JAR file already closed";
        try
          let location = (String.concat "/" (pack @ [name]) ^ ".class") in
          let entry = Zip.find_entry zip location in
          let data = Zip.read_entry zip entry in
          Some (IO.input_string data), file, file ^ "@" ^ location
        with
          | Not_found -> 
            None, file, file),
      (fun () -> closed := true; Zip.close_in zip)
  in
  let rec build path p =
    match get_raw_class path, path with
    | (None, _, _), ([], c) -> build (["haxe";"root"], c) p
    | (None, _, _), _ -> None
    | (Some i, real_path, pos_path), _ ->
      (*try*)
        let pos = { pfile = pos_path; pmin = 0; pmax = 0; } in
        let cls = JReader.parse_class i in
        let pack = match fst path with | ["haxe";"root"] -> [] | p -> p in
        Some ( real_path, (pack, [convert_java_class pos cls, pos]) )
      (*with JReader.Error_message msg ->
        if com.verbose then prerr_endline ("Class reader failed: " ^ msg);
        None
        | _ -> None*)
  in

  (* TODO: add_dependency m mdep *)
  com.load_extern_type <- com.load_extern_type @ [build];
  com.java_libs <- (file, close) :: com.java_libs