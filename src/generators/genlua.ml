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

open Ast
open Type
open Common
open ExtList
open Error

type pos = Globals.pos

type ctx = {
    com : Common.context;
    buf : Buffer.t;
    packages : (string list,unit) Hashtbl.t;
    mutable current : tclass;
    mutable statics : (tclass * string * texpr) list;
    mutable inits : texpr list;
    mutable tabs : string;
    mutable in_value : tvar option;
    mutable in_loop : bool;
    mutable in_loop_try : bool;
    mutable iife_assign : bool;
    mutable break_depth : int;
    mutable handle_continue : bool;
    mutable id_counter : int;
    mutable type_accessor : module_type -> string;
    mutable separator : bool;
    mutable found_expose : bool;
    mutable lua_jit : bool;
    mutable lua_ver : float;
}

type object_store = {
    os_name : string;
    mutable os_fields : object_store list;
}


let debug_expression expression  =
    " --[[ " ^ Type.s_expr_kind expression  ^ " --]] "

let debug_type t  =
    " --[[ " ^ Type.s_type_kind t  ^ " --]] ";;

let flat_path (p,s) =
    (* Replace _ with __ in paths to prevent name collisions. *)
    let escape str = String.concat "_" (ExtString.String.nsplit str "_") in

    match p with
    | [] -> escape s
    | _ -> "__" ^ String.concat "_" (List.map escape p) ^ "_" ^ (escape s)

let get_exposed ctx path meta = try
        let (_, args, pos) = Meta.get Meta.Expose meta in
        (match args with
         | [ EConst (String (s,_)), _ ] -> [s]
         | [] -> [path]
         | _ -> error "Invalid @:expose parameters" pos)
    with Not_found -> []

let dot_path = Globals.s_type_path

let s_path ctx = flat_path

(* Lua requires decimal encoding for characters, rather than the hex *)
(* provided by Ast.s_escape *)
let s_escape_lua ?(dec=true) s =
    let b = Buffer.create (String.length s) in
    for i = 0 to (String.length s) - 1 do
        match s.[i] with
        | '\n' -> Buffer.add_string b "\\n"
        | '\t' -> Buffer.add_string b "\\t"
        | '\r' -> Buffer.add_string b "\\r"
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | c when int_of_char c < 32 && dec ->
            Buffer.add_string b (Printf.sprintf "\\%.3d" (int_of_char c))
        | c -> Buffer.add_char b c
    done;
    Buffer.contents b

(* TODO: are all these kwds necessary for field quotes *and* id escapes? *)
let kwds =
    let h = Hashtbl.create 0 in
    List.iter (fun s -> Hashtbl.add h s ()) [
        "_G"; ""; "and"; "break"; "do"; "else"; "elseif";
        "end"; "false"; "for"; "function"; "if";
        "in"; "local"; "nil"; "not"; "or"; "repeat";
        "return"; "then"; "true"; "until"; "while";
        "goto"; "self";
    ];
    h

let valid_lua_ident s =
    try
        for i = 0 to String.length s - 1 do
            match String.unsafe_get s i with
            | 'a'..'z' | 'A'..'Z' | '_' -> ()
            | '0'..'9' when i > 0 -> ()
            | _ -> raise Exit
        done;
        true
    with Exit ->
        false

let field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "[\"" ^ s ^ "\"]" else "." ^ s
let ident s = if Hashtbl.mem kwds s then "_" ^ s else s

let anon_field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "['" ^ (s_escape_lua s) ^ "']" else s
let static_field c s =
    match s with
    | "length" | "name" when not c.cl_extern || Meta.has Meta.HxGen c.cl_meta-> "._hx" ^ s
    | s -> field s

let has_feature ctx = Common.has_feature ctx.com
let add_feature ctx = Common.add_feature ctx.com

let temp ctx =
    ctx.id_counter <- ctx.id_counter + 1;
    "_hx_" ^ string_of_int (ctx.id_counter)

let spr ctx s =
    ctx.separator <- false;
    Buffer.add_string ctx.buf s

let print ctx =
    ctx.separator <- false;
    Printf.kprintf (fun s -> begin
            Buffer.add_string ctx.buf s
        end)

let newline ctx = print ctx "\n%s" ctx.tabs

(* print with newline *)
let println ctx =
    ctx.separator <- false;
    Printf.kprintf (fun s -> begin
            Buffer.add_string ctx.buf s;
            newline ctx
        end)

let unsupported p = error "This expression cannot be compiled to Lua" p

let basename path =
    try
        let idx = String.rindex path '/' in
        String.sub path (idx + 1) (String.length path - idx - 1)
    with Not_found -> path

(* TODO : is this necessary any more?*)
let newprop ctx =
    match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
    | '{' -> print ctx "\n%s" ctx.tabs
    | _ -> print ctx "\n%s" ctx.tabs

let semicolon ctx =
    match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
    | '}' when not ctx.separator -> ()
    | _ -> spr ctx ";"

let rec concat ctx s f = function
    | [] -> ()
    | [x] -> f x
    | x :: l ->
        f x;
        spr ctx s;
        concat ctx s f l

let fun_block ctx f p =
    let e = List.fold_left (fun e (a,c) ->
        match c with
        | None | Some TNull -> e
        | Some c -> Type.concat (Codegen.set_default ctx.com a c p) e
    ) f.tf_expr f.tf_args in
    e

let open_block ctx =
    let oldt = ctx.tabs in
    ctx.tabs <- "  " ^ ctx.tabs;
    (fun() -> ctx.tabs <- oldt)

let this ctx = match ctx.in_value with None -> "self" | Some _ -> "self"

let is_dot_access e cf =
    match follow(e.etype), cf with
    | TInst (c,_), FInstance(_,_,icf)  when (Meta.has Meta.LuaDotMethod c.cl_meta || Meta.has Meta.LuaDotMethod icf.cf_meta)->
        true;
    | _ ->
        false

let is_dynamic_iterator ctx e =
    let check x =
        has_feature ctx "HxOverrides.iter" && (match follow x.etype with
            | TInst ({ cl_path = [],"Array" },_)
            | TInst ({ cl_kind = KTypeParameter _}, _)
            | TAnon _
            | TDynamic _
            | TMono _ ->
                true
            | _ -> false
        )
    in
    match e.eexpr with
    | TField (x,f) when field_name f = "iterator" -> check x
    | _ ->
        false

(*
	return index of a first element in the list for which `f` returns true
	TODO: is there some standard function to do that?
 *)
let index_of f l =
    let rec find lst idx =
        match lst with
        | [] -> raise Not_found
        | el :: rest ->
            if f el then
                idx
            else
                find rest (idx + 1)
    in
    find l 0

(* create a __lua__ call *)
let mk_lua_code com code args t pos =
    let lua_local = mk (TIdent "__lua__") t_dynamic pos in
    let code_const = Codegen.ExprBuilder.make_string com code pos in
    mk (TCall (lua_local, code_const :: args)) t pos

(* create a multi-return boxing call for given expr *)
let mk_mr_box ctx e =
    let s_fields =
        match follow e.etype with
        | TInst (c,_) ->
            String.concat ", " (List.map (fun f -> "\"" ^ f.cf_name ^ "\"") c.cl_ordered_fields)
        | _ -> assert false
    in
    add_feature ctx "use._hx_box_mr";
    add_feature ctx "use._hx_table";
    let code = Printf.sprintf "_hx_box_mr(_hx_table.pack({0}), {%s})" s_fields in
    mk_lua_code ctx.com code [e] e.etype e.epos

(* create a multi-return select call for given expr and field name *)
let mk_mr_select com e ecall name =
    let i =
        match follow ecall.etype with
        | TInst (c,_) ->
            index_of (fun f -> f.cf_name = name) c.cl_ordered_fields
        | _ ->
            assert false
    in
    if i == 0 then
        mk_lua_code com "{0}" [ecall] e.etype e.epos
    else
        let code = Printf.sprintf "_G.select(%i, {0})" (i + 1) in
        mk_lua_code com code [ecall] e.etype e.epos

(* from genphp *)
let rec is_string_type t =
    match follow t with
    | TInst ({cl_path = ([], "String")}, _) -> true
    | TAnon a ->
        (match !(a.a_status) with
         | Statics ({cl_path = ([], "String")}) -> true
         | _ -> false)
    | TAbstract (a,pl) -> is_string_type (Abstract.get_underlying_type a pl)
    | _ -> false

let is_string_expr e = is_string_type e.etype
(* /from genphp *)

let is_dynamic t = match follow t with
    | TMono _ | TDynamic _
    | TInst({ cl_kind = KTypeParameter _ }, _) -> true
    | TAnon anon ->
        (match !(anon.a_status) with
         | EnumStatics _ | Statics _ -> false
         | _ -> true
        )
    | _ -> false

let is_dynamic_expr e = is_dynamic e.etype

let is_structural_type t =
    match follow t with
    | TDynamic _ -> true
    | TAnon a -> true
    | TType ({t_type = TAnon _},_) -> true
    | _ -> false

let rec is_int_type ctx t =
    match follow t with
    | TInst ({cl_path = ([], "Int")}, _) -> true
    | TAnon a ->
        (match !(a.a_status) with
         | Statics ({cl_path = ([], "Int")}) -> true
         | _ -> false)
    | TAbstract ({a_path = ([],"Float")}, pl) -> false
    | TAbstract ({a_path = ([],"Int")}, pl) -> true
    | TAbstract (a,pl) -> is_int_type ctx (Abstract.get_underlying_type a pl)
    | _ -> false

let rec extract_expr e = match e.eexpr with
    | TParenthesis e
    | TMeta (_,e)
    | TCast(e,_) -> extract_expr e
    | _ -> e

let gen_constant ctx p = function
    | TInt i -> print ctx "%ld" i
    | TFloat s -> spr ctx s
    | TString s -> begin
            add_feature ctx "use.string";
            print ctx "\"%s\"" (s_escape_lua s)
        end
    | TBool b -> spr ctx (if b then "true" else "false")
    | TNull -> spr ctx "nil"
    | TThis -> spr ctx (this ctx)
    | TSuper -> assert false

let rec gen_call ctx e el =
    ctx.iife_assign <- true;
    (match e.eexpr , el with
     | TConst TSuper , params ->
         (match ctx.current.cl_super with
          | None -> error "Missing api.setCurrentClass" e.epos
          | Some (c,_) ->
              print ctx "%s.super(%s" (ctx.type_accessor (TClassDecl c)) (this ctx);
              List.iter (fun p -> print ctx ","; gen_value ctx p) params;
              spr ctx ")";
         );
     | TField ({ eexpr = TConst TSuper },f) , params ->
         (match ctx.current.cl_super with
          | None -> error "Missing api.setCurrentClass" e.epos
          | Some (c,_) ->
              let name = field_name f in
              print ctx "%s.prototype%s(%s" (ctx.type_accessor (TClassDecl c)) (field name) (this ctx);
              List.iter (fun p -> print ctx ","; gen_value ctx p) params;
              spr ctx ")";
         );
     | TCall (x,_) , el when (match x.eexpr with TIdent "__lua__" -> false | _ -> true) ->
         gen_paren ctx [e];
         gen_paren ctx el;
     | TIdent "__new__", { eexpr = TConst (TString cl) } :: params ->
         print ctx "%s.new(" cl;
         concat ctx "," (gen_value ctx) params;
         spr ctx ")";
     | TIdent "__new__", e :: params ->
         gen_value ctx e;
         spr ctx ".new(";
         concat ctx "," (gen_value ctx) params;
         spr ctx ")";
     | TIdent "__callself__", { eexpr = TConst (TString head) } :: { eexpr = TConst (TString tail) } :: el ->
         print ctx "%s:%s" head tail;
         gen_paren ctx el;
     | TIdent "__call__", { eexpr = TConst (TString code) } :: el ->
         spr ctx code;
         gen_paren ctx el;
     | TIdent "__lua_length__", [e]->
         spr ctx "#"; gen_value ctx e;
     | TIdent "__lua_table__", el ->
         let count = ref 0 in
         spr ctx "({";
         List.iter (fun e ->
             (match e with
              | { eexpr = TArrayDecl arr } ->
                  if (!count > 0 && List.length(arr) > 0) then spr ctx ",";
                  concat ctx "," (gen_value ctx) arr;
                  if List.length(arr) > 0 then incr count;
              | { eexpr = TObjectDecl fields } ->
                  if (!count > 0 && List.length(fields) > 0) then spr ctx ",";
                  concat ctx ", " (fun ((f,_,_),e) ->
                      print ctx "%s = " (anon_field f);
                      gen_value ctx e
                  ) fields;
                  if List.length(fields) > 0 then incr count;
              | { eexpr = TConst(TNull)} -> ()
              | _ ->
                  error "__lua_table__ only accepts array or anonymous object arguments" e.epos;
             )) el;
         spr ctx "})";
     | TIdent "__lua__", [{ eexpr = TConst (TString code) }] ->
         spr ctx (String.concat "\n" (ExtString.String.nsplit code "\r\n"))
     | TIdent "__lua__", { eexpr = TConst (TString code); epos = p } :: tl ->
         Codegen.interpolate_code ctx.com code tl (spr ctx) (gen_expr ctx) p
     | TIdent "__type__",  [o] ->
         spr ctx "type";
         gen_paren ctx [o];
     | TIdent "__define_feature__", [_;e] ->
         gen_expr ctx e
     | TIdent "__feature__", { eexpr = TConst (TString f) } :: eif :: eelse ->
         (if has_feature ctx f then
              gen_value ctx eif
          else match eelse with
              | [] -> ()
              | e :: _ -> gen_value ctx e)
     | TIdent "__resources__", [] ->
         (* TODO: Array declaration helper *)
         spr ctx "_hx_tab_array({";
         let count = ref 0 in
         concat ctx "," (fun (name,data) ->
             if (!count == 0) then spr ctx "[0]=";
             spr ctx "{ ";
             spr ctx "name = ";
             gen_constant ctx e.epos (TString name);
             spr ctx ", data = ";
             gen_constant ctx e.epos (TString (Codegen.bytes_serialize data));
             spr ctx "}";
             incr count
         ) (Hashtbl.fold (fun name data acc -> (name,data) :: acc) ctx.com.resources []);
         print ctx "}, %i)" !count;
     | TIdent "`trace", [e;infos] ->
         if has_feature ctx "haxe.Log.trace" then begin
             let t = (try List.find (fun t -> t_path t = (["haxe"],"Log")) ctx.com.types with _ -> assert false) in
             spr ctx (ctx.type_accessor t);
             spr ctx ".trace(";
             gen_value ctx e;
             spr ctx ",";
             gen_value ctx infos;
             spr ctx ")";
         end else begin
             spr ctx "_hx_print(";
             gen_value ctx e;
             spr ctx ")";
         end
     | TField ( { eexpr = TConst(TInt _ | TFloat _| TString _| TBool _) } as e , ((FInstance _ | FAnon _) as ef)), el ->
         gen_paren ctx [e];
         print ctx (":%s(") (field_name ef);
         concat ctx "," (gen_value ctx) el;
         spr ctx ")";
     | TField (_, FStatic( { cl_path = ([],"Std") }, { cf_name = "string" })),[{eexpr = TCall({eexpr=TField (_, FStatic( { cl_path = ([],"Std") }, { cf_name = "string" }))}, _)} as el] ->
         (* unwrap recursive Std.string(Std.string(...)) declarations to Std.string(...) *)
         gen_value ctx el;
     | TField ({eexpr = TLocal _} as e, ef), el when is_possible_string_field e (field_name ef)  ->
         add_feature ctx "use._hx_wrap_if_string_field";
         add_feature ctx "use.string";
         spr ctx "_hx_wrap_if_string_field(";
         gen_value ctx e;
         print ctx ",'%s'" (field_name ef);
         spr ctx ")(";
         concat ctx "," (gen_value ctx) (e::el);
         spr ctx ")";
     | TField (e, ((FInstance _ | FAnon _ | FDynamic _) as ef)), el ->
         let s = (field_name ef) in
         if Hashtbl.mem kwds s || not (valid_lua_ident s) then begin
             add_feature ctx "use._hx_apply_self";
             spr ctx "_hx_apply_self(";
             gen_value ctx e;
             print ctx ",\"%s\"" (field_name ef);
             if List.length(el) > 0 then spr ctx ",";
             concat ctx "," (gen_value ctx) el;
             spr ctx ")";
         end else begin
             gen_value ctx e;
             if is_dot_access e ef then
                 print ctx ".%s" (field_name ef)
             else
                 print ctx ":%s" (field_name ef);
             gen_paren ctx el;
         end;
     | _ ->
         gen_value ctx e;
         gen_paren ctx el);
    ctx.iife_assign <- false;

and has_continue e =
    let rec loop e = match e.eexpr with
        | TContinue -> raise Exit
        | TWhile(e1,_,_) | TFor(_,e1,_) -> loop e1 (* in theory there could be a continue there. Note that we don't want to recurse into the loop body because we do not care about inner continue expressions *)
        | _ -> Type.iter loop e
    in
    try
        loop e;
        false;
    with Exit ->
        true

and gen_paren ctx el =
    spr ctx "(";
    concat ctx ", " (gen_value ctx) el;
    spr ctx ")";

and gen_cond ctx cond =
    ctx.iife_assign <- true;
    gen_value ctx cond;
    ctx.iife_assign <- false

and gen_loop ctx label cond e =
    let old_in_loop = ctx.in_loop in
    ctx.in_loop <- true;
    let old_handle_continue = ctx.handle_continue in
    let will_continue = has_continue e in
    ctx.handle_continue <- has_continue e;
    ctx.break_depth <- ctx.break_depth + 1;
    if will_continue then begin
        println ctx "local _hx_continue_%i = false;" ctx.break_depth;
    end;
    let b = open_block ctx in
    print ctx "%s " label;
    gen_cond ctx cond;
    print ctx " do ";
    if will_continue then print ctx "repeat ";
    gen_block_element ctx e;
    if will_continue then begin
        if will_continue then begin
            println ctx "until true";
        end;
        println ctx "if _hx_continue_%i then " ctx.break_depth;
        println ctx "_hx_continue_%i = false;" ctx.break_depth;
        if ctx.in_loop_try then
            println ctx "_G.error(\"_hx_pcall_break\");"
        else
            println ctx "break;";
        println ctx "end;";
    end;
    b();
    newline ctx;
    print ctx "end";
    ctx.in_loop <- old_in_loop;
    ctx.break_depth <- ctx.break_depth-1;
    ctx.handle_continue <- old_handle_continue;


and is_possible_string_field e field_name=
    (* Special case for String fields *)
    let structural_type = is_structural_type e.etype in
    if not structural_type then
        false
    else match field_name with
        | "length"
        | "toUpperCase"
        | "toLowerCase"
        | "charAt"
        | "indexOf"
        | "lastIndexOf"
        | "split"
        | "toString"
        | "substring"
        | "substr"
        | "charCodeAt" ->
            true
        | _ ->
            false


and gen_expr ?(local=true) ctx e = begin
    match e.eexpr with
      TConst c ->
        gen_constant ctx e.epos c;
    | TLocal v when v.v_name = "this" ->
        spr ctx "self";
    | TLocal v -> spr ctx (ident v.v_name)
    | TArray (e1,{ eexpr = TConst (TString s) }) when valid_lua_ident s && (match e1.eexpr with TConst (TInt _|TFloat _) -> false | _ -> true) ->
        gen_value ctx e1;
        spr ctx (field s)
    | TArray (e1,e2) ->
        gen_value ctx e1;
        spr ctx "[";
        gen_value ctx e2;
        spr ctx "]";
    | TBinop (op,e1,e2) ->
        gen_tbinop ctx op e1 e2;
    | TField (x,f) when field_name f = "iterator" && is_dynamic_iterator ctx e ->
        add_feature ctx "use._iterator";
        print ctx "_iterator(";
        gen_value ctx x;
        print ctx ")";
    | TField (x,FClosure (_,f)) ->
        add_feature ctx "use._hx_bind";
        (match x.eexpr with
         | TConst _ | TLocal _ ->
             print ctx "_hx_bind(";
             gen_value ctx x;
             print ctx ",";
             gen_value ctx x;
             print ctx "%s)" (if Meta.has Meta.SelfCall f.cf_meta then "" else (field f.cf_name))
         | _ ->
             print ctx "(function() local __=";
             gen_value ctx x;
             print ctx "; return _hx_bind(__,__%s) end)()" (if Meta.has Meta.SelfCall f.cf_meta then "" else (field f.cf_name)))
    | TEnumParameter (x,_,i) ->
        gen_value ctx x;
        print ctx "[%i]" (i + 2)
    | TEnumIndex x ->
        gen_value ctx x;
        print ctx "[1]"
    | TField (e, ef) when is_string_expr e && field_name ef = "length"->
        spr ctx "#";
        gen_value ctx e;
    | TField (e, ef) when is_possible_string_field e (field_name ef)  ->
        add_feature ctx "use._hx_wrap_if_string_field";
        add_feature ctx "use.string";
        spr ctx "_hx_wrap_if_string_field(";
        gen_value ctx e;
        print ctx ",'%s')" (field_name ef)
    | TField (x, (FInstance(_,_,f) | FStatic(_,f) | FAnon(f))) when Meta.has Meta.SelfCall f.cf_meta ->
        gen_value ctx x;
    | TField ({ eexpr = TConst(TInt _ | TFloat _| TString _| TBool _) } as e , ((FInstance _ | FAnon _) as ef)) ->
        gen_paren ctx [e];
        print ctx (".%s") (field_name ef);
    | TField ({ eexpr = TConst (TInt _ | TFloat _) } as x,f) ->
        gen_expr ctx { e with eexpr = TField(mk (TParenthesis x) x.etype x.epos,f) }
    | TField ({ eexpr = TObjectDecl fields }, ef ) ->
        spr ctx "(function(x) return x.";
        print ctx "%s" (field_name ef);
        spr ctx " end )({";
        concat ctx ", " (fun ((f,_,_),e) -> print ctx "%s = " (anon_field f); gen_value ctx e) fields;
        spr ctx "})";
    | TField ({eexpr = TLocal v}, f) when Meta.has Meta.MultiReturn v.v_meta ->
        (* field of a multireturn local var is actually just a local var *)
        let (_, args, pos) =  Meta.get (Meta.Custom ":lua_mr_id") v.v_meta  in
        (match args with
         | [(EConst(String(id,_)), _)] ->
             spr ctx (id ^ "_" ^ (ident v.v_name) ^ "_" ^ (field_name f));
         | _ ->
             assert false);
    | TField (x,f) ->
        gen_value ctx x;
        let name = field_name f in
        spr ctx (match f with FStatic _ | FEnum _ | FInstance _ | FAnon _ | FDynamic _ | FClosure _ -> field name)
    | TTypeExpr t ->
        spr ctx (ctx.type_accessor t)
    | TParenthesis e ->
        gen_paren ctx [e];
    | TMeta (_,e) ->
        gen_expr ctx e
    | TReturn eo -> gen_return ctx e eo;
    | TBreak ->
        if not ctx.in_loop then unsupported e.epos;
        if ctx.handle_continue then
            print ctx "_hx_continue_%i = true;" ctx.break_depth;
        if ctx.in_loop_try then
            print ctx "_G.error(\"_hx_pcall_break\", 0)"
        else
            spr ctx "break"
    | TContinue ->
        if not ctx.in_loop then unsupported e.epos;
        if ctx.in_loop_try then
            print ctx "_G.error(\"_hx_pcall_break\", 0)"
        else
            spr ctx "break" (*todo*)
    | TBlock el ->
        let bend = open_block ctx in
        List.iter (gen_block_element ctx) el;
        bend();
        newline ctx;
    | TFunction f ->
        let old = ctx.in_value, ctx.in_loop in
        ctx.in_value <- None;
        ctx.in_loop <- false;
        print ctx "function(%s) " (String.concat "," (List.map ident (List.map arg_name f.tf_args)));
        let fblock = fun_block ctx f e.epos in
        (match fblock.eexpr with
         | TBlock el ->
             let bend = open_block ctx in
             List.iter (gen_block_element ctx) el;
             bend();
             newline ctx;
         |_ -> ());
        spr ctx "end";
        ctx.in_value <- fst old;
        ctx.in_loop <- snd old;
        ctx.separator <- true
    | TCall (e,el) ->
        gen_call ctx e el;
    | TArrayDecl el ->
        spr ctx "_hx_tab_array({";
        let count = ref 0 in
        List.iteri (fun i e ->
            incr count;
            if (i == 0) then spr ctx "[0]="
            else spr ctx ", ";
            gen_value ctx e) el;
        print ctx "}, %i)" !count;
    | TThrow e ->
        spr ctx "_G.error(";
        gen_value ctx e;
        spr ctx ",0)";
    | TVar (v,eo) ->
        begin match eo with
            | None ->
                if local then
                    spr ctx "local ";
                spr ctx (ident v.v_name);
            | Some e ->
                match e.eexpr with
                | TBinop(OpAssign, e1, e2) ->
                    gen_tbinop ctx OpAssign e1 e2;
                    if local then
                        spr ctx " local ";
                    spr ctx (ident v.v_name);
                    spr ctx " = ";
                    gen_value ctx e1;

                | _ when Meta.has Meta.MultiReturn v.v_meta ->
                    (* multi-return var is generated as several vars for unpacking *)
                    let id = temp ctx in
                    let temp_expr = (EConst(String(id,Double)), Globals.null_pos) in
                    v.v_meta <- (Meta.Custom ":lua_mr_id", [temp_expr], v.v_pos) :: v.v_meta;
                    let name = ident v.v_name in
                    let names =
                        match follow v.v_type with
                        | TInst (c, _) ->
                            List.map (fun f -> id ^ "_" ^name ^ "_" ^ f.cf_name) c.cl_ordered_fields
                        | _ ->
                            assert false
                    in
                    spr ctx "local ";
                    spr ctx (String.concat ", " names);
                    spr ctx " = ";
                    gen_value ctx e;

                | _ ->
                    if local then
                        spr ctx "local ";
                    spr ctx (ident v.v_name);
                    spr ctx " = ";

                    (* if it was a multi-return var but it was used as a value itself, *)
                    (* we have to box it in an object conforming to a multi-return extern class *)
                    let is_boxed_multireturn = Meta.has (Meta.Custom ":lua_mr_box") v.v_meta in
                    let e = if is_boxed_multireturn then mk_mr_box ctx e else e in
                    gen_value ctx e;
        end
    | TNew (c,_,el) ->
        (match c.cl_constructor with
         | Some cf when Meta.has Meta.SelfCall cf.cf_meta ->
             print ctx "%s" (ctx.type_accessor (TClassDecl c));
         | Some cf when Meta.has Meta.Native cf.cf_meta ->
             let _, args, mp = Meta.get Meta.Native cf.cf_meta in
             (match args with
              | [( EConst(String (s,_)),_)] ->
                  print ctx "%s.%s" (ctx.type_accessor (TClassDecl c)) s;
              | _ ->
                  print ctx "%s.new" (ctx.type_accessor (TClassDecl c)));
         | _ -> print ctx "%s.new" (ctx.type_accessor (TClassDecl c)));
        gen_paren ctx el;
    | TIf (cond,e,eelse) ->
        ctx.iife_assign <- true;
        spr ctx "if ";
        gen_cond ctx cond;
        spr ctx " then ";
        let bend = open_block ctx in
        gen_block_element ctx e;
        bend();
        newline ctx;
        (match eelse with
         | None -> ();
         | Some e2 ->
             (match e.eexpr with
              | TObjectDecl _ -> ctx.separator <- false
              | _ ->());
             spr ctx "else";
             let bend = open_block ctx in
             gen_block_element ctx e2;
             bend();
             newline ctx);
        spr ctx "end";
        ctx.iife_assign <- false;
    | TUnop ((Increment|Decrement) as op,unop_flag, e) ->
        (* TODO: Refactor this mess *)
        println ctx "(function() ";
        (match e.eexpr, unop_flag with
         | TArray(e1,e2), _ ->
             spr ctx "local _hx_idx = "; gen_value ctx e2; semicolon ctx; newline ctx;
             spr ctx "local _hx_arr ="; gen_value ctx e1; semicolon ctx; newline ctx;
             (match unop_flag with
              | Ast.Postfix ->
                  spr ctx "local _ = _hx_arr[_hx_idx]"; semicolon ctx; newline ctx;
              | _ -> ());
             spr ctx "_hx_arr[_hx_idx] = _hx_arr[_hx_idx]";
         | TField(e1,e2), _ ->
             spr ctx "local _hx_obj = "; gen_value ctx e1; semicolon ctx; newline ctx;
             spr ctx "local _hx_fld = ";
             (match e2 with
              | FInstance(_,_,fld)
              | FStatic(_,fld)
              | FAnon fld
              | FClosure(_,fld) ->
                  print ctx "'%s'" fld.cf_name;
              | FDynamic name ->
                  print ctx "'%s'" name;
              | FEnum(_,efld) ->
                  print ctx "'%s'" efld.ef_name);
             semicolon ctx; newline ctx;
             (match unop_flag with
              | Ast.Postfix ->
                  spr ctx "local _ = _hx_obj[_hx_fld]"; semicolon ctx; newline ctx;
              | _ -> ());
             spr ctx "_hx_obj[_hx_fld] = _hx_obj[_hx_fld] ";
         | _, Ast.Prefix ->
             gen_value ctx e;
             spr ctx " = ";
             gen_value ctx e;
         | _, Ast.Postfix ->
             spr ctx "local _ = ";
             gen_value ctx e; semicolon ctx;
             gen_value ctx e;
             spr ctx " = ";
             gen_value ctx e);
        (match op with
         |Increment -> spr ctx " + 1;"
         |Decrement -> spr ctx " - 1;"
         |_-> print ctx " %s 1;" (Ast.s_unop op));
        newline ctx;
        spr ctx " return ";
        (match unop_flag, e.eexpr with
         | Ast.Postfix, _ ->
             spr ctx "_";
         | _, TArray(e1,e2) ->
             spr ctx "_hx_arr[_hx_idx]";
         | _, TField(e1,e2) ->
             spr ctx "_hx_obj[_hx_fld]";
         | _, _ ->
             gen_value ctx e;
        );
        semicolon ctx; newline ctx;
        spr ctx " end)()";
    | TUnop (Not,unop_flag,e) ->
        spr ctx "not ";
        gen_value ctx e;
    | TUnop (NegBits,unop_flag,e) ->
        add_feature ctx "use._bitop";
        spr ctx "_hx_bit.bnot(";
        gen_value ctx e;
        spr ctx ")";
    | TUnop (op,Ast.Prefix,e) ->
        spr ctx (Ast.s_unop op);
        gen_value ctx e
    | TUnop (op,Ast.Postfix,e) ->
        gen_value ctx e;
        spr ctx (Ast.s_unop op)
    | TWhile (cond,e,Ast.NormalWhile) ->
        gen_loop ctx "while" cond e
    | TWhile (cond,e,Ast.DoWhile) ->
        println ctx "while true do ";
        gen_block_element ctx e;
        gen_loop ctx "while" cond e
    | TObjectDecl [] ->
        spr ctx "_hx_e()";
        ctx.separator <- true
    | TObjectDecl fields ->
        spr ctx "_hx_o({__fields__={";
        concat ctx "," (fun ((f,_,_),e) -> print ctx "%s=" (anon_field f); spr ctx "true") fields;
        spr ctx "},";
        concat ctx "," (fun ((f,_,_),e) -> print ctx "%s=" (anon_field f); gen_anon_value ctx e) fields;
        spr ctx "})";
        ctx.separator <- true
    | TFor (v,it,e2) ->
        unsupported e.epos;
    | TTry (e,catchs) ->
        (* TODO: add temp variables *)
        let old_in_loop_try = ctx.in_loop_try in
        if ctx.in_loop then
            ctx.in_loop_try <- true;
        println ctx "local _hx_status, _hx_result = pcall(function() ";
        let b = open_block ctx in
        gen_expr ctx e;
        let vname = temp ctx in
        b();
        println ctx "return _hx_pcall_default";
        println ctx "end)";
        ctx.in_loop_try <- old_in_loop_try;
        println ctx "if not _hx_status and _hx_result == \"_hx_pcall_break\" then";
        if ctx.in_loop then
            if old_in_loop_try then
                println ctx "  _G.error(_hx_result,0);"
            else
                println ctx "  break";
        println ctx "elseif not _hx_status then ";
        let bend = open_block ctx in
        newline ctx;
        print ctx "local %s = _hx_result" vname;
        let last = ref false in
        let else_block = ref false in
        List.iter (fun (v,e) ->
            if !last then () else
                let t = (match follow v.v_type with
                    | TEnum (e,_) -> Some (TEnumDecl e)
                    | TInst (c,_) -> Some (TClassDecl c)
                    | TAbstract (a,_) -> Some (TAbstractDecl a)
                    | TFun _
                    | TLazy _
                    | TType _
                    | TAnon _ ->
                        assert false
                    | TMono _
                    | TDynamic _ ->
                        None
                ) in
                match t with
                | None ->
                    last := true;
                    if !else_block then print ctx "";
                    if vname <> v.v_name then begin
                        newline ctx;
                        print ctx "local %s = %s" v.v_name vname;
                    end;
                    gen_block_element ctx e;
                    if !else_block then begin
                        newline ctx;
                        print ctx " end ";
                    end
                | Some t ->
                    if not !else_block then newline ctx;
                    print ctx "if( %s.__instanceof(%s," (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" })) vname;
                    gen_value ctx (mk (TTypeExpr t) (mk_mono()) e.epos);
                    spr ctx ") ) then ";
                    let bend = open_block ctx in
                    if vname <> v.v_name then begin
                        newline ctx;
                        print ctx "local %s = %s" v.v_name vname;
                    end;
                    gen_block_element ctx e;
                    bend();
                    newline ctx;
                    spr ctx "else";
                    else_block := true
        ) catchs;
        if not !last then begin
            println ctx " _G.error(%s)" vname;
            spr ctx "end";
        end;
        bend();
        newline ctx;
        println ctx "elseif _hx_result ~= _hx_pcall_default then";
        println ctx "  return _hx_result";
        print ctx "end";
    | TSwitch (e,cases,def) ->
        List.iteri (fun cnt (el,e2) ->
            if cnt == 0 then spr ctx "if "
            else (newline ctx; spr ctx "elseif ");
            List.iteri (fun ccnt e3 ->
                if ccnt > 0 then spr ctx " or ";
                gen_value ctx e;
                spr ctx " == ";
                gen_value ctx e3;
            ) el;
            print ctx " then ";
            let bend = open_block ctx in
            gen_block_element ctx e2;
            bend();
        ) cases;
        (match def with
         | None -> spr ctx " end"
         | Some e ->
             begin
                 if (List.length(cases) > 0) then
                     spr ctx "else";
                 let bend = open_block ctx in
                 bend();
                 gen_block_element ctx e;
                 if (List.length(cases) > 0) then
                     spr ctx " end";
             end;);
    | TCast (e1,Some t) ->
        print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
        gen_expr ctx e1;
        spr ctx " , ";
        spr ctx (ctx.type_accessor t);
        spr ctx ")"
    | TCast (e1,None) ->
        gen_value ctx e1;
    | TIdent s ->
        spr ctx s
end;

    (* gen_block_element handles expressions that map to "statements" in lua. *)
    (* It handles no-op situations, and ensures that expressions are formatted with newlines *)
and gen_block_element ctx e  =
    ctx.iife_assign <- false;
    begin match e.eexpr with
        | TTypeExpr _ | TConst _ | TLocal _ | TFunction _ ->
            ()
        | TCast (e',_) | TParenthesis e' | TMeta (_,e') ->
            gen_block_element ctx e'
        | TArray (e1,e2) ->
            gen_block_element ctx e1;
            gen_block_element ctx e2;
        | TArrayDecl el | TBlock el ->
            List.iter (gen_block_element ctx) el;
            (* For plain lua table instantiations, just capture argument operations *)
        | TCall({ eexpr = TIdent "__lua_table__"} , el) ->
            List.iter(fun x -> gen_block_element ctx x) el
        (* make a no-op __define_feature__ expression possible *)
        | TCall({eexpr = TIdent "__define_feature__"}, [_;e]) ->
            gen_block_element ctx e
        | TObjectDecl fl ->
            List.iter (fun (_,e) -> gen_block_element ctx e) fl
        | TBinop (op,e1,e2) when op <> Ast.OpAssign ->
            newline ctx;
            let f () = gen_tbinop ctx op e1 e2 in
            gen_iife_assign ctx f;
        | TUnop ((Increment|Decrement) as op,_,e) ->
            newline ctx;
            gen_expr ctx e;
            print ctx " = ";
            gen_expr ctx e;
            (match op with
             | Increment -> print ctx " + 1;"
             | _ -> print ctx " - 1;"
            )
        | TSwitch (e,[],def) ->
            (match def with
             | None -> ()
             | Some e -> gen_block_element ctx e)
        | TField _ ->
            newline ctx;
            let f () = gen_expr ctx e in
            gen_iife_assign ctx f;
            semicolon ctx;
        | TCall ({ eexpr = TIdent "__feature__" }, { eexpr = TConst (TString f) } :: eif :: eelse) ->
            if has_feature ctx f then
                gen_block_element ctx eif
            else (match eelse with
                | [] -> ()
                | [e] -> gen_block_element ctx e
                | _ -> assert false)
        | _ ->
            newline ctx;
            gen_expr ctx e;
            semicolon ctx;
    end;

and is_const_null e =
    match e.eexpr with
    | TConst TNull ->
        true
    | _ ->
        false

    (* values generated in anon structures can get modified.  Functions are bind-ed *)
    (* and include a dummy "self" leading variable so they can be called like normal *)
    (* instance methods *)
and gen_anon_value ctx e =
    match e with
    | { eexpr = TFunction f} ->
        let old = ctx.in_value, ctx.in_loop in
        ctx.in_value <- None;
        ctx.in_loop <- false;
        print ctx "function(%s) " (String.concat "," ("self" :: (List.map ident (List.map arg_name f.tf_args))));
        let fblock = fun_block ctx f e.epos in
        (match fblock.eexpr with
         | TBlock el ->
             let bend = open_block ctx in
             List.iter (gen_block_element ctx) el;
             bend();
             newline ctx;
         |_ -> ());
        spr ctx "end";
        ctx.in_value <- fst old;
        ctx.in_loop <- snd old;
        ctx.separator <- true
    | _ when (is_function_type ctx e.etype) && not (is_const_null e) ->
        spr ctx "function(_,...) return ";
        gen_value ctx e;
        spr ctx "(...) end";
    | _->
        gen_value ctx e

and gen_value ctx e =
    let assign e =
        mk (TBinop (Ast.OpAssign,
                    mk (TLocal (match ctx.in_value with None -> assert false | Some v -> v)) t_dynamic e.epos,
                    e
                   )) e.etype e.epos
    in
    let value() =
        let old = ctx.in_value, ctx.in_loop in
        let r_id = temp ctx in
        let r = alloc_var r_id t_dynamic e.epos in
        ctx.in_value <- Some r;
        ctx.in_loop <- false;
        spr ctx "(function() ";
        let b = open_block ctx in
        newline ctx;
        println ctx "local %s" r_id;
        (fun() ->
             newline ctx;
             spr ctx ("return " ^ r_id);
             b();
             newline ctx;
             ctx.in_value <- fst old;
             ctx.in_loop <- snd old;
             spr ctx "end )()"
        )
    in
    match e.eexpr with
    | TBinop (OpAssign, e1, e2) ->
        spr ctx "(function() ";
        gen_block_element ctx e;
        spr ctx " return ";
        gen_value ctx e1;
        spr ctx " end)()";
    | TConst _
    | TLocal _
    | TArray _
    | TBinop _
    | TField _
    | TEnumParameter _
    | TEnumIndex _
    | TTypeExpr _
    | TParenthesis _
    | TObjectDecl _
    | TArrayDecl _
    | TNew _
    | TUnop _
    | TFunction _
    | TIdent _ ->
        gen_expr ctx e
    | TMeta (_,e1) ->
        gen_value ctx e1
    | TCall (e,el) ->
        gen_call ctx e el
    | TReturn _
    | TBreak
    | TContinue ->
        unsupported e.epos
    (* TODO: this is just a hack because this specific case is a TestReflect unit test. I don't know how to address this properly
       	   at the moment. - Simon *)
    | TCast ({ eexpr = TTypeExpr mt } as e1, None) when (match mt with TClassDecl {cl_path = ([],"Array")} -> false | _ -> true) ->
        add_feature ctx "use._hx_staticToInstance";
        spr ctx "_hx_staticToInstance(";
        gen_expr ctx e1;
        spr ctx ")";
    | TCast (e1, Some t) ->
        print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
        gen_value ctx e1;
        spr ctx " , ";
        spr ctx (ctx.type_accessor t);
        spr ctx ")"
    | TCast (e1, _) ->
        gen_value ctx e1
    | TVar _
    | TFor _
    | TWhile _
    | TThrow _ ->
        (* value is discarded anyway *)
        let v = value() in
        gen_expr ctx e;
        v()
    | TBlock [e] ->
        gen_value ctx e
    | TBlock el ->
        let v = value() in
        let rec loop = function
            | [] ->
                spr ctx "return nil";
            | [e] ->
                gen_block_element ctx (assign e);
            | e :: l ->
                gen_block_element ctx e;
                newline ctx;
                loop l
        in
        loop el;
        v();
    | TIf (cond,e,eo) ->
        let v = value() in
        spr ctx "if ";
        gen_cond ctx cond;
        spr ctx " then ";
        gen_block_element ctx (assign e);
        let rec gen_elseif ctx e =
            (match e with
             | None->();
             | Some e2->
                 (match e2.eexpr with
                  | TIf(cond3, e3, eo3) ->
                      spr ctx " elseif ";
                      gen_cond ctx cond3;
                      spr ctx " then ";
                      gen_block_element ctx (assign e3);
                      gen_elseif ctx eo3;
                  | _ ->
                      spr ctx " else ";
                      gen_block_element ctx (assign e2);
                 ));
        in
        gen_elseif ctx eo;
        spr ctx " end";
        v()
    | TSwitch (cond,cases,def) ->
        let v = value() in
        gen_expr ctx (mk (TSwitch (cond,
                                   List.map (fun (e1,e2) -> (e1,assign e2)) cases,
                                   match def with None -> None | Some e -> Some (assign e)
                                  )) e.etype e.epos);
        v()
    | TTry (b,catchs) ->
        let v = value() in
        let block e = mk (TBlock [e]) e.etype e.epos in
        gen_block_element ctx (mk (TTry (block (assign b),
                                         List.map (fun (v,e) -> v, block (assign e)) catchs
                                        )) e.etype e.epos);
        v()

and is_function_type ctx t =
    match follow(t) with
    | TFun _ -> true
    | _ -> false;

and gen_tbinop ctx op e1 e2 =
    (match op, e1.eexpr, e2.eexpr with
     | Ast.OpAssign, TField(e3, FInstance _), TFunction f ->
         gen_expr ctx e1;
         spr ctx " = " ;
         print ctx "function(%s) " (String.concat "," ("self" :: List.map ident (List.map arg_name f.tf_args)));
         let fblock = fun_block ctx f e1.epos in
         (match fblock.eexpr with
          | TBlock el ->
              let rec loop ctx el = (match el with
                  | [hd] -> (match hd.eexpr with
                      | TReturn eo -> begin
                              newline ctx;
                              gen_return ctx e1 eo;
                          end;
                      | _ -> gen_block_element ctx hd);
                  | hd :: tl ->
                      gen_block_element ctx hd;
                      loop ctx tl
                  |[] ->()
              ) in
              let bend = open_block ctx in
              loop ctx el;
              bend();
              newline ctx;
          | _ -> gen_value ctx e2);
         spr ctx " end"
     | Ast.OpAssign, _, _ ->
         let iife_assign = ctx.iife_assign in
         if iife_assign then spr ctx "(function() ";
         (match e1.eexpr, e2.eexpr with
          | _, TBinop(OpAssign as op, e3, e4) ->
              gen_tbinop ctx op e3 e4;
              newline ctx;
              gen_value ctx e1;
              spr ctx " = ";
              gen_value ctx e3;
          | TField(e3, (FInstance _| FClosure _ | FAnon _ ) ), TField(e4, (FClosure _| FStatic _ | FAnon _) )  ->
              gen_value ctx e1;
              print ctx " %s " (Ast.s_binop op);
              add_feature ctx "use._hx_funcToField";
              spr ctx "_hx_funcToField(";
              gen_value ctx e2;
              spr ctx ")";
          | TField(_, FInstance _ ), TLocal t  when (is_function_type ctx t.v_type)   ->
              gen_value ctx e1;
              print ctx " %s " (Ast.s_binop op);
              add_feature ctx "use._hx_funcToField";
              spr ctx "_hx_funcToField(";
              gen_value ctx e2;
              spr ctx ")";
          | _ ->
              gen_value ctx e1;
              print ctx " %s " (Ast.s_binop op);
              gen_value ctx e2);
         if iife_assign then begin
             spr ctx " return ";
             gen_value ctx e1;
             spr ctx " end)()";
         end;
     | Ast.OpAssignOp(op2), TArray(e3,e4), _ ->
         (* TODO: Figure out how to rewrite this expression more cleanly *)
         println ctx "(function() ";
         let idx = alloc_var "idx" e4.etype e4.epos in
         let idx_var =  mk (TVar( idx , Some(e4))) e4.etype e4.epos in
         gen_expr ctx idx_var;
         let arr = alloc_var "arr" e3.etype e3.epos in
         let arr_var = mk (TVar(arr, Some(e3))) e3.etype e3.epos in
         gen_expr ctx arr_var;
         newline ctx;
         let arr_expr = (mk (TArray(
             (mk ( TLocal(arr)) e3.etype e3.epos),
             (mk ( TLocal(idx)) e4.etype e4.epos)
         )) e3.etype e3.epos) in
         spr ctx "arr[idx] = ";
         gen_tbinop ctx op2 arr_expr e2; semicolon ctx; newline ctx;
         spr ctx "return arr[idx]";
         spr ctx " end)()";
     | Ast.OpAssignOp(op2), TField(e3,e4), _ ->
         (* TODO: Figure out how to rewrite this expression more cleanly *)
         println ctx "(function() ";
         let obj = alloc_var "obj" e3.etype e3.epos in
         spr ctx "local fld = ";
         (match e4 with
          | FInstance(_,_,fld)
          | FStatic(_,fld)
          | FAnon fld
          | FClosure(_,fld) ->
              print ctx "'%s'" fld.cf_name;
          | FDynamic name ->
              print ctx "'%s'" name;
          | FEnum(_,efld) ->
              print ctx "'%s'" efld.ef_name);
         semicolon ctx; newline ctx;
         let obj_var = mk (TVar(obj, Some(e3))) e3.etype e3.epos in
         gen_expr ctx obj_var;
         newline ctx;
         let obj_expr = (mk (TField(
             (mk ( TLocal(obj)) e3.etype e3.epos),
             e4
         )) e3.etype e3.epos) in
         spr ctx "obj[fld] = ";
         gen_tbinop ctx op2 obj_expr e2; semicolon ctx; newline ctx;
         spr ctx "return obj[fld]";
         spr ctx " end)()";
     | Ast.OpAssignOp(op2),_,_ ->
         (* TODO: Rewrite expression *)
         spr ctx "(function() "; gen_value ctx e1;
         spr ctx " = "; gen_tbinop ctx op2 e1 e2;
         spr ctx " return "; gen_value ctx e1;
         spr ctx " end)()";
     | Ast.OpXor,_,_ | Ast.OpAnd,_,_  | Ast.OpShl,_,_ | Ast.OpShr,_,_ | Ast.OpUShr,_,_ | Ast.OpOr,_,_ ->
         gen_bitop ctx op e1 e2;
     | Ast.OpMod,_,_ ->
         spr ctx "_G.math.fmod(";
         gen_expr ctx e1;
         spr ctx ", ";
         gen_expr ctx e2;
         spr ctx ")";
     | Ast.OpAdd, _, _ when (is_dynamic_expr e1 && is_dynamic_expr e2) ->
         add_feature ctx "use._hx_dyn_add";
         spr ctx "_hx_dyn_add(";
         gen_value ctx e1;
         spr ctx ",";
         gen_value ctx e2;
         spr ctx ")";
     | Ast.OpAdd,_,_ when (is_string_expr e1 || is_string_expr e2) ->
         spr ctx "Std.string(";
         gen_value ctx e1;
         spr ctx ") .. Std.string(";
         gen_value ctx e2;
         spr ctx ")";
     | _ -> begin
             (* wrap expressions used in comparisons for lua *)
             gen_paren_tbinop ctx e1;
             (match op with
              | Ast.OpNotEq -> print ctx " ~= ";
              | Ast.OpBoolAnd -> print ctx " and ";
              | Ast.OpBoolOr -> print ctx " or ";
              | _ -> print ctx " %s " (Ast.s_binop op));
             gen_paren_tbinop ctx e2;
         end;
    );

and gen_paren_tbinop ctx e =
    let ee = extract_expr e in
    match ee.eexpr with
    | TBinop _  ->
        gen_paren ctx [ee];
    | _ ->
        gen_value ctx ee

and gen_bitop ctx op e1 e2 =
    add_feature ctx "use._bitop";
    print ctx "_hx_bit.%s(" (match op with
        | Ast.OpXor  ->  "bxor"
        | Ast.OpAnd  ->  "band"
        | Ast.OpShl  ->  "lshift"
        | Ast.OpShr  ->  "arshift"
        | Ast.OpUShr ->  "rshift"
        | Ast.OpOr   ->  "bor"
        | _ -> "");
    gen_value ctx e1;
    spr ctx ",";
    gen_value ctx e2;
    spr ctx ")"

and gen_return ctx e eo =
    if ctx.in_value <> None then unsupported e.epos;
    (match eo with
     | None ->
         spr ctx "do return end"
     | Some e ->
         (match e.eexpr with
          | TField (e2, ((FClosure (_, tcf) | FAnon tcf |FInstance (_,_,tcf)))) when is_function_type ctx tcf.cf_type ->
              (* See issue #6259 *)
              add_feature ctx "use._hx_bind";
              spr ctx "do return ";
              print ctx "_hx_bind(";
              gen_value ctx e2;
              spr ctx ",";
              gen_value ctx e;
              spr ctx ") end";
          | TBinop(OpAssign, e1, e2) ->
              gen_expr ctx e;
              spr ctx " do return ";
              gen_value ctx e1;
              spr ctx " end";
          | _ ->
              spr ctx "do return ";
              gen_value ctx e;
              spr ctx " end");
    )

and gen_iife_assign ctx f =
    spr ctx "(function() return ";
    f();
    spr ctx " end)()";


and has_class ctx c =
    has_feature ctx "lua.Boot.getClass" && (c.cl_super <> None || c.cl_ordered_fields <> [] || c.cl_constructor <> None)

and has_prototype ctx c =
    c.cl_super <> None || (has_class ctx c) || List.exists (can_gen_class_field ctx) c.cl_ordered_fields

and can_gen_class_field ctx = function
    | { cf_expr = (None | Some { eexpr = TConst TNull }) } when not (has_feature ctx "Type.getInstanceFields") ->
        false
    | f ->
        is_physical_field f


let check_multireturn ctx c =
    match c with
    | _ when Meta.has Meta.MultiReturn c.cl_meta ->
        if not c.cl_extern then
            error "MultiReturns must be externs" c.cl_pos
        else if List.length c.cl_ordered_statics > 0 then
            error "MultiReturns must not contain static fields" c.cl_pos
        else if (List.exists (fun cf -> match cf.cf_kind with Method _ -> true | _-> false) c.cl_ordered_fields) then
            error "MultiReturns must not contain methods" c.cl_pos;
    | {cl_super = Some(csup,_)} when Meta.has Meta.MultiReturn csup.cl_meta ->
        error "Cannot extend a MultiReturn" c.cl_pos
    | _ -> ()


let check_field_name c f =
    match f.cf_name with
    | "prototype" | "__proto__" | "constructor" ->
        error ("The field name '" ^ f.cf_name ^ "'  is not allowed in Lua") (match f.cf_expr with None -> c.cl_pos | Some e -> e.epos);
    | _ -> ()

(* convert a.b.c to ["a"]["b"]["c"] *)
let path_to_brackets path =
    let parts = ExtString.String.nsplit path "." in
    "[\"" ^ (String.concat "\"][\"" parts) ^ "\"]"

let gen_class_static_field ctx c f =
    match f.cf_expr with
    | None | Some { eexpr = TConst TNull } when not (has_feature ctx "Type.getClassFields") ->
        ()
    | None when not (is_physical_field f) ->
        ()
    | None ->
        println ctx "%s%s = nil" (s_path ctx c.cl_path) (field f.cf_name);
    | Some e ->
        match e.eexpr with
        | TFunction _ ->
            let path = (s_path ctx c.cl_path) ^ (field f.cf_name) in
            let dot_path = (dot_path c.cl_path) ^ (static_field c f.cf_name) in
            ctx.id_counter <- 0;
            print ctx "%s = " path;
            gen_value ctx e;
            newline ctx;
            (match (get_exposed ctx dot_path f.cf_meta) with [s] -> (print ctx "_hx_exports%s = %s" (path_to_brackets s) path; newline ctx) | _ -> ());
        | _ ->
            ctx.statics <- (c,f.cf_name,e) :: ctx.statics

let gen_class_field ctx c f =
    let p = s_path ctx c.cl_path in
    check_field_name c f;
    print ctx "%s.prototype%s" p (field f.cf_name);
    match f.cf_expr with
    | None ->
        println ctx "= nil;"
    | Some e ->
        ctx.id_counter <- 0;
        (match e.eexpr with
         | TFunction f2 ->
             let old = ctx.in_value, ctx.in_loop in
             ctx.in_value <- None;
             ctx.in_loop <- false;
             print ctx " = function";
             print ctx "(%s) " (String.concat "," ("self" :: List.map ident (List.map arg_name f2.tf_args)));
             let fblock = fun_block ctx f2 e.epos in
             (match fblock.eexpr with
              | TBlock el ->
                  let rec loop ctx el = (match el with
                      | [hd] -> (match hd.eexpr with
                          | TReturn eo -> begin
                                  newline ctx;
                                  gen_return ctx e eo;
                              end;
                          | _ -> gen_block_element ctx hd);
                      | hd :: tl ->
                          gen_block_element ctx hd;
                          loop ctx tl
                      |[] ->()
                  ) in
                  let bend = open_block ctx in
                  loop ctx el;
                  bend();
                  newline ctx;
              |_ -> ());
             println ctx "end";
             ctx.in_value <- fst old;
             ctx.in_loop <- snd old;
             ctx.separator <- true;
         | _ ->
             gen_value ctx e;
             newline ctx;
        )

let generate_class___name__ ctx c =
    if has_feature ctx "lua.Boot.isClass" then begin
        let p = s_path ctx c.cl_path in
        print ctx "%s.__name__ = " p;
        if has_feature ctx "Type.getClassName" then
            println ctx "{%s}" (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" (s_escape_lua s)) (fst c.cl_path @ [snd c.cl_path])))
        else
            println ctx "true";
    end

let generate_class ctx c =
    ctx.current <- c;
    ctx.id_counter <- 0;
    (match c.cl_path with
     | [],"Function" -> error "This class redefines a native one" c.cl_pos
     | _ -> ());
    let p = s_path ctx c.cl_path in
    let hxClasses = has_feature ctx "Type.resolveClass" in
    newline ctx;
    print ctx "%s.new = " p;
    (match c.cl_kind with
     | KAbstractImpl _ ->
         (* abstract implementations only contain static members and don't need to have constructor functions *)
         print ctx "{}"; ctx.separator <- true
     | _ ->
         (match c.cl_constructor with
          | Some { cf_expr = Some e } ->
              (match e.eexpr with
               | TFunction f ->
                   let old = ctx.in_value, ctx.in_loop in
                   ctx.in_value <- None;
                   ctx.in_loop <- false;
                   print ctx "function(%s) " (String.concat "," (List.map ident (List.map arg_name f.tf_args)));
                   let fblock = fun_block ctx f e.epos in
                   (match fblock.eexpr with
                    | TBlock el ->
                        let bend = open_block ctx in
                        newline ctx;
                        if not (has_prototype ctx c) then println ctx "local self = _hx_new()" else
                            println ctx "local self = _hx_new(%s.prototype)" p;
                        println ctx "%s.super(%s)" p (String.concat "," ("self" :: (List.map ident (List.map arg_name f.tf_args))));
                        if p = "String" then println ctx "self = string";
                        spr ctx "return self";
                        bend(); newline ctx;
                        spr ctx "end"; newline ctx;
                        let bend = open_block ctx in
                        print ctx "%s.super = function(%s) " p (String.concat "," ("self" :: (List.map ident (List.map arg_name f.tf_args))));
                        List.iter (gen_block_element ctx) el;
                        bend();
                        newline ctx;
                        spr ctx "end";
                    |_ -> ());
                   ctx.in_value <- fst old;
                   ctx.in_loop <- snd old;
                   ctx.separator <- true
               | _ -> gen_expr ctx e);
          | _ -> (print ctx "{}"); ctx.separator <- true)
    );
    newline ctx;

    (match (get_exposed ctx (s_path ctx c.cl_path) c.cl_meta) with [s] -> (print ctx "_hx_exports%s = %s" (path_to_brackets s) p; newline ctx) | _ -> ());

    if hxClasses then println ctx "_hxClasses[\"%s\"] = %s" (dot_path c.cl_path) p;
    generate_class___name__ ctx c;
    (match c.cl_implements with
     | [] -> ()
     | l ->
         println ctx "%s.__interfaces__ = {%s}" p (String.concat "," (List.map (fun (i,_) -> ctx.type_accessor (TClassDecl i)) l));
    );

    let gen_props props =
        String.concat "," (List.map (fun (p,v) -> p ^"=\""^v^"\"") props) in
    let has_property_reflection =
        (has_feature ctx "Reflect.getProperty") || (has_feature ctx "Reflect.setProperty") in

    if has_property_reflection then begin
        (match Codegen.get_properties c.cl_ordered_statics with
         | [] -> ()
         | props -> println ctx "%s.__properties__ = {%s}" p (gen_props props);
        );
    end;

    List.iter (gen_class_static_field ctx c) c.cl_ordered_statics;

    if (has_prototype ctx c) then begin
        println ctx "%s.prototype = _hx_a();" p;
        let count = ref 0 in
        List.iter (fun f -> if can_gen_class_field ctx f then (gen_class_field ctx c f) ) c.cl_ordered_fields;
        if (has_class ctx c) then begin
            newprop ctx;
            println ctx "%s.prototype.__class__ =  %s" p p;
            incr count;
        end;

        if has_property_reflection then begin
            let props = Codegen.get_properties c.cl_ordered_fields in
            (match c.cl_super with
             | _ when props = [] -> ()
             | _ ->
                 newprop ctx;
                 println ctx "%s.prototype.__properties__ =  {%s}" p (gen_props props));
        end;
        (match c.cl_super with
         | None -> ()
         | Some (csup,_) ->
             let psup = ctx.type_accessor (TClassDecl csup) in
             println ctx "%s.__super__ = %s" p psup;
             println ctx "setmetatable(%s.prototype,{__index=%s.prototype})" p psup;
             (* Also use the __properties__  from the super class as the __index metatable *)
             if has_property_reflection && Codegen.has_properties csup then
                 println ctx "setmetatable(%s.prototype.__properties__,{__index=%s.prototype.__properties__})" p psup;
        );
    end

let generate_enum ctx e =
    let p = s_path ctx e.e_path in
    let ename = List.map (fun s -> Printf.sprintf "\"%s\"" (s_escape_lua s)) (fst e.e_path @ [snd e.e_path]) in

    (* TODO: Unify the _hxClasses declaration *)
    if has_feature ctx "Type.resolveEnum" then begin
        print ctx "_hxClasses[\"%s\"] = %s" (dot_path e.e_path) p; semicolon ctx; newline ctx;
    end;
    if has_feature ctx "lua.Boot.isEnum" then begin
        print ctx "_hxClasses[\"%s\"] = {" (dot_path e.e_path);
        if has_feature ctx "lua.Boot.isEnum" then  begin
            print ctx " __ename__ = %s," (if has_feature ctx "Type.getEnumName" then "{" ^ String.concat "," ename ^ "}" else "true");
        end;
        (* TODO :  Come up with a helper function for _hx_tab_array declarations *)
        spr ctx " __constructs__ = _hx_tab_array({";
        if ((List.length e.e_names) > 0) then begin
            spr ctx "[0]=";
            spr ctx (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" s) e.e_names));
        end;
        print ctx "},%i)}" (List.length e.e_names);
        ctx.separator <- true;
        newline ctx;
    end;

    if has_feature ctx "Type.resolveEnum" || has_feature ctx "lua.Boot.isEnum" then
        print ctx "%s = _hxClasses[\"%s\"];" p (dot_path e.e_path);

    newline ctx;
    List.iter (fun n ->
        let f = PMap.find n e.e_constrs in
        print ctx "%s%s = " p (field f.ef_name);
        (match f.ef_type with
         | TFun (args,_) ->
             let count = List.length args in
             let sargs = String.concat "," (List.map (fun (n,_,_) -> ident n) args) in
             print ctx "function(%s) local _x = _hx_tab_array({[0]=\"%s\",%d,%s,__enum__=%s}, %i);" sargs f.ef_name f.ef_index sargs p (count + 2);
             if has_feature ctx "may_print_enum" then
                 (* TODO: better namespacing for _estr *)
                 spr ctx " rawset(_x, 'toString', _estr);";
             spr ctx " return _x; end ";
             ctx.separator <- true;
         | _ ->
             println ctx "_hx_tab_array({[0]=\"%s\",%d,__enum__ = %s},2)" f.ef_name f.ef_index p;
             if has_feature ctx "may_print_enum" then begin
                 println ctx "rawset(%s%s, 'toString', _estr)" p (field f.ef_name);
             end;
        );
        newline ctx
    ) e.e_names;
    if has_feature ctx "Type.allEnums" then begin
        let ctors_without_args = List.filter (fun s ->
            let ef = PMap.find s e.e_constrs in
            match follow ef.ef_type with
            | TFun _ -> false
            | _ -> true
        ) e.e_names in
        print ctx "%s.__empty_constructs__ = " p;
        spr ctx "_hx_tab_array({";
        if (List.length ctors_without_args)  > 0 then
            begin
                spr ctx "[0] = ";
                print ctx "%s" (String.concat "," (List.map (fun s -> Printf.sprintf "%s.%s" p s) ctors_without_args));
            end;
        println ctx "}, %i)"  (List.length ctors_without_args);
    end

let generate_static ctx (c,f,e) =
    print ctx "%s%s = " (s_path ctx c.cl_path) (field f);
    gen_value ctx e;
    semicolon ctx;
    newline ctx

let generate_enumMeta_fields ctx = function
    | TEnumDecl e -> begin
            let p = s_path ctx e.e_path in
            match Codegen.build_metadata ctx.com (TEnumDecl e) with
            | None -> ()
            | Some e ->
                print ctx "%s.__meta__ = " p;
                gen_expr ctx e;
                newline ctx
        end
    | _ -> ()

let generate_require ctx path meta =
    let _, args, mp = Meta.get Meta.LuaRequire meta in
    let p = (s_path ctx path) in

    (match args with
     | [(EConst(String(module_name,_)),_)] ->
         print ctx "%s = _G.require(\"%s\")" p module_name
     | [(EConst(String(module_name,_)),_) ; (EConst(String(object_path,_)),_)] ->
         print ctx "%s = _G.require(\"%s\").%s" p module_name object_path
     | _ ->
         error "Unsupported @:luaRequire format" mp);

    newline ctx

let generate_type ctx = function
    | TClassDecl c ->
        (match c.cl_init with
         | None -> ()
         | Some e ->
             ctx.inits <- e :: ctx.inits);
        let p = s_path ctx c.cl_path in
        (* A special case for Std because we do not want to generate it if it's empty. *)
        if p = "Std" && c.cl_ordered_statics = [] then
            ()
        else if (not c.cl_extern) && Meta.has Meta.LuaDotMethod c.cl_meta then
            error "LuaDotMethod is valid for externs only" c.cl_pos
        else if not c.cl_extern then
            generate_class ctx c;
        check_multireturn ctx c;
    | TEnumDecl e ->
        if not e.e_extern then generate_enum ctx e
        else ();
    | TTypeDecl _ | TAbstractDecl _ -> ()

let generate_type_forward ctx = function
    | TClassDecl c ->
        if not c.cl_extern then
            begin
                let p = s_path ctx c.cl_path in
                let l,c = c.cl_path in
                if List.length(l) == 0 then spr ctx "local ";
                println ctx "%s = _hx_e()" p
            end
        else if Meta.has Meta.LuaRequire c.cl_meta && is_directly_used ctx.com c.cl_meta then
            generate_require ctx c.cl_path c.cl_meta
    | TEnumDecl e when e.e_extern ->
        if Meta.has Meta.LuaRequire e.e_meta && is_directly_used ctx.com e.e_meta then
            generate_require ctx e.e_path e.e_meta;
    | TEnumDecl e ->
        let p = s_path ctx e.e_path in
        let l,c = e.e_path in
        if List.length(l) == 0 then spr ctx "local ";
        println ctx "%s = _hx_e()" p;
    | TTypeDecl _ | TAbstractDecl _ -> ()

let alloc_ctx com =
    let ctx = {
        com = com;
        buf = Buffer.create 16000;
        packages = Hashtbl.create 0;
        statics = [];
        inits = [];
        current = null_class;
        tabs = "";
        in_value = None;
        iife_assign = false;
        in_loop = false;
        in_loop_try = false;
        break_depth = 0;
        handle_continue = false;
        id_counter = 0;
        type_accessor = (fun _ -> assert false);
        separator = false;
        found_expose = false;
        lua_jit = Common.defined com Define.LuaJit;
        lua_ver = try
                float_of_string (PMap.find "lua_ver" com.defines.Define.values)
            with | Not_found -> 5.2;
    } in
    ctx.type_accessor <- (fun t ->
        let p = t_path t in
        match t with
        | TClassDecl ({ cl_extern = true } as c) when not (Meta.has Meta.LuaRequire c.cl_meta)
            -> dot_path p
        | TEnumDecl { e_extern = true }
            -> s_path ctx p
        | _ -> s_path ctx p);
    ctx


let transform_multireturn ctx = function
    | TClassDecl c ->
        let transform_field f =
            match f.cf_expr with
            | Some e ->
                let rec loop e =
                    let is_multireturn t =
                        match follow t with
                        | TInst (c, _) when Meta.has Meta.MultiReturn c.cl_meta -> true
                        | _ -> false
                    in
                    match e.eexpr with
     (*
						if we found a var declaration initialized by a multi-return call, mark it with @:multiReturn meta,
						so it will later be generated as multiple locals unpacking the value
					*)
                    | TVar (v, Some ({ eexpr = TCall _ } as ecall)) when is_multireturn v.v_type ->
                        v.v_meta <- (Meta.MultiReturn,[],v.v_pos) :: v.v_meta;
                        let ecall = Type.map_expr loop ecall in
                        { e with eexpr = TVar (v, Some ecall) }

                    (* if we found a field access for the multi-return call, generate select call *)
                    | TField ({ eexpr = TCall _ } as ecall, f) when is_multireturn ecall.etype ->
                        let ecall = Type.map_expr loop ecall in
                        mk_mr_select ctx.com e ecall (field_name f)

                    (* if we found a multi-return call used as a value, box it *)
                    | TCall _ when is_multireturn e.etype ->
                        let e = Type.map_expr loop e in
                        mk_mr_box ctx e

                    (* Don't bother wrapping multireturn function results if we don't use the return values *)
                    | TBlock el ->
                        let el2 = List.map (fun x ->
                            match x.eexpr with
                            | TCall (e2, el) when is_multireturn x.etype ->
                                mk (TCall (e2, List.map(fun x-> Type.map_expr loop x) el)) x.etype x.epos
                            | _ -> loop x) el in
                        mk (TBlock el2) e.etype e.epos;


                        (* if we found a field access for a multi-return local - that's fine, because it'll be generated as a local var *)
                    | TField ({ eexpr = TLocal v}, _) when Meta.has Meta.MultiReturn v.v_meta ->
                        e
                    | TReturn Some(e2) ->
                        if is_multireturn e2.etype then
                            error "You cannot return a multireturn type from a haxe function" e2.epos
                        else
                            Type.map_expr loop e;
     (*
						if we found usage of local var we previously marked with @:multiReturn as a value itself,
						remove the @:multiReturn meta and add "box me" meta so it'll be boxed on var initialization
					*)
                    | TLocal v when Meta.has Meta.MultiReturn v.v_meta ->
                        v.v_meta <- List.filter (fun (m,_,_) -> m <> Meta.MultiReturn) v.v_meta;
                        v.v_meta <- (Meta.Custom ":lua_mr_box", [], v.v_pos) :: v.v_meta;
                        e

                    | _ ->
                        Type.map_expr loop e
                in
                f.cf_expr <- Some (loop e);
            | _ -> ()
        in
        List.iter transform_field c.cl_ordered_fields;
        List.iter transform_field c.cl_ordered_statics;
        Option.may transform_field c.cl_constructor;
    | _ -> ()

let generate com =
    let ctx = alloc_ctx com in

    Codegen.map_source_header com (fun s -> print ctx "-- %s\n" s);

    if has_feature ctx "Class" || has_feature ctx "Type.getClassName" then add_feature ctx "lua.Boot.isClass";
    if has_feature ctx "Enum" || has_feature ctx "Type.getEnumName" then add_feature ctx "lua.Boot.isEnum";

    let include_files = List.rev com.include_files in
    List.iter (fun file ->
        match file with
        | path, "top" ->
            let file_content = Std.input_file ~bin:true (fst file) in
            print ctx "%s\n" file_content;
            ()
        | _ -> ()
    ) include_files;

    let var_exports = (
        "_hx_exports",
        "_hx_exports or {}"
    ) in

    let exposed = List.concat (List.map (fun t ->
        match t with
        | TClassDecl c ->
            let path = dot_path c.cl_path in
            let class_exposed = get_exposed ctx path c.cl_meta in
            let static_exposed = List.map (fun f ->
                get_exposed ctx (path ^ static_field c f.cf_name) f.cf_meta
            ) c.cl_ordered_statics in
            List.concat (class_exposed :: static_exposed)
        | _ -> []
    ) com.types) in
    let anyExposed = exposed <> [] in
    let exportMap = ref (PMap.create String.compare) in
    let exposedObject = { os_name = ""; os_fields = [] } in
    let toplevelExposed = ref [] in
    List.iter (fun path -> (
            let parts = ExtString.String.nsplit path "." in
            let rec loop p pre = match p with
                | f :: g :: ls ->
                    let path = match pre with "" -> f | pre -> (pre ^ "." ^ f) in
                    if not (PMap.exists path !exportMap) then (
                        let elts = { os_name = f; os_fields = [] } in
                        exportMap := PMap.add path elts !exportMap;
                        let cobject = match pre with "" -> exposedObject | pre -> PMap.find pre !exportMap in
                        cobject.os_fields <- elts :: cobject.os_fields
                    );
                    loop (g :: ls) path;
                | f :: [] when pre = "" ->
                    toplevelExposed := f :: !toplevelExposed;
                | _ -> ()
            in loop parts "";
        )) exposed;


    if (anyExposed) then (
        print ctx "local %s = %s" (fst var_exports) (snd var_exports);
        ctx.separator <- true;
        newline ctx
    );

    let rec print_obj f root = (
        let path = root ^ (path_to_brackets f.os_name) in
        print ctx "%s = %s or _hx_e()" path path;
        ctx.separator <- true;
        newline ctx;
        concat ctx ";" (fun g -> print_obj g path) f.os_fields
    )
    in
    List.iter (fun f -> print_obj f "_hx_exports") exposedObject.os_fields;


    let vars = [] in
    (* let vars = (if has_feature ctx "Type.resolveClass" || has_feature ctx "Type.resolveEnum" then ("_hxClasses = " ^ "{}") :: vars else vars) in *)
    let vars = if has_feature ctx "may_print_enum"
        then ("_estr = function(self) return " ^ (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" })) ^ ".__string_rec(self,''); end") :: vars
        else vars in
    (match List.rev vars with
     | [] -> ()
     | vl ->
         print ctx "local %s" (String.concat ";" vl);
         ctx.separator <- true;
         newline ctx
    );

    List.iter (generate_type_forward ctx) com.types; newline ctx;

    (* Generate some dummy placeholders for utility libs that may be required*)
    println ctx "local _hx_bind, _hx_bit, _hx_staticToInstance, _hx_funcToField, _hx_maxn, _hx_print, _hx_apply_self, _hx_box_mr, _hx_bit_clamp, _hx_table, _hx_bit_raw";
    println ctx "local _hx_pcall_default = {};";
    println ctx "local _hx_pcall_break = {};";

    List.iter (transform_multireturn ctx) com.types;
    List.iter (generate_type ctx) com.types;

    if has_feature ctx "use._bitop" || has_feature ctx "lua.Boot.clamp" then begin
        println ctx "_hx_bit_clamp = function(v) ";
        println ctx "  if v <= 2147483647 and v >= -2147483648 then";
        println ctx "    if v > 0 then return _G.math.floor(v)";
        println ctx "    else return _G.math.ceil(v)";
        println ctx "    end";
        println ctx "  end";
        println ctx "  if v > 2251798999999999 then v = v*2 end;";
        println ctx "  if (v ~= v or math.abs(v) == _G.math.huge) then return nil end";
        println ctx "  return _hx_bit.band(v, 2147483647 ) - math.abs(_hx_bit.band(v, 2147483648))";
        println ctx "end";
        println ctx "pcall(require, 'bit')"; (* require this for lua 5.1 *)
        println ctx "if bit then";
        println ctx "  _hx_bit = bit";
        println ctx "elseif bit32 then";
        println ctx "  local _hx_bit_raw = bit32";
        println ctx "  _hx_bit = setmetatable({}, { __index = _hx_bit_raw });";
        println ctx "  _hx_bit.bnot = function(...) return _hx_bit_clamp(_hx_bit_raw.bnot(...)) end;"; (* lua 5.2  weirdness *)
        println ctx "  _hx_bit.bxor = function(...) return _hx_bit_clamp(_hx_bit_raw.bxor(...)) end;"; (* lua 5.2  weirdness *)
        println ctx "else";
        println ctx "  _G.error(\"Bitop library is missing.  Please install luabitop\");";
        println ctx "end";
    end;

    (* Array is required, always patch it *)
    println ctx "_hx_array_mt.__index = Array.prototype";
    newline ctx;

    let b = open_block ctx in
    println ctx "local _hx_static_init = function()";
    (* Generate statics *)
    List.iter (generate_static ctx) (List.rev ctx.statics);
    (* Localize init variables inside a do-block *)
    (* Note: __init__ logic can modify static variables. *)
    (* Generate statics *)
    List.iter (gen_block_element ctx) (List.rev ctx.inits);
    b();
    newline ctx;
    println ctx "end";
    newline ctx;

    if has_feature ctx "use._iterator" then begin
        add_feature ctx "use._hx_bind";
        println ctx "function _hx_iterator(o)  if ( lua.Boot.__instanceof(o, Array) ) then return function() return HxOverrides.iter(o) end elseif (typeof(o.iterator) == 'function') then return  _hx_bind(o,o.iterator) else return  o.iterator end end";
    end;

    if has_feature ctx "use._hx_bind" then begin
        println ctx "_hx_bind = function(o,m)";
        println ctx "  if m == nil then return nil end;";
        println ctx "  local f;";
        println ctx "  if o._hx__closures == nil then";
        println ctx "    _G.rawset(o, '_hx__closures', {});";
        println ctx "  else ";
        println ctx "    f = o._hx__closures[m];";
        println ctx "  end";
        println ctx "  if (f == nil) then";
        println ctx "    f = function(...) return m(o, ...) end;";
        println ctx "    o._hx__closures[m] = f;";
        println ctx "  end";
        println ctx "  return f;";
        println ctx "end";
    end;

    if has_feature ctx "use._hx_staticToInstance" then begin
        println ctx "_hx_staticToInstance = function (tab)";
        println ctx "  return setmetatable({}, {";
        println ctx "    __index = function(t,k)";
        println ctx "      if type(rawget(tab,k)) == 'function' then ";
        println ctx "	return function(self,...)";
        println ctx "	  return rawget(tab,k)(...)";
        println ctx "	end";
        println ctx "      else";
        println ctx "	return rawget(tab,k)";
        println ctx "      end";
        println ctx "    end";
        println ctx "  })";
        println ctx "end";
    end;

    if has_feature ctx "use._hx_funcToField" then begin
        println ctx "_hx_funcToField = function(f)";
        println ctx "  if type(f) == 'function' then ";
        println ctx "    return function(self,...) ";
        println ctx "      return f(...) ";
        println ctx "    end";
        println ctx "  else ";
        println ctx "    return f";
        println ctx "  end";
        println ctx "end";
    end;

    if has_feature ctx "Math.random" then begin
        println ctx "_G.math.randomseed(_G.os.time());"
    end;

    if has_feature ctx "use._hx_print" then
        println ctx "_hx_print = print or (function() end)";

    if has_feature ctx "use._hx_apply_self" then begin
        println ctx "_hx_apply_self = function(self, f, ...)";
        println ctx "  return self[f](self,...)";
        println ctx "end";
    end;

    if has_feature ctx "use._hx_box_mr" then begin
        println ctx "_hx_box_mr = function(x,nt)";
        println ctx "   res = _hx_o({__fields__={}})";
        println ctx "   for i,v in ipairs(nt) do";
        println ctx "     res[v] = x[i]";
        println ctx "   end";
        println ctx "   return res";
        println ctx "end";
    end;

    if has_feature ctx "use._hx_table" then begin
        println ctx "_hx_table = {}";
        println ctx "_hx_table.pack = _G.table.pack or function(...)";
        println ctx "    return {...}";
        println ctx "end";
        println ctx "_hx_table.unpack = _G.table.unpack or _G.unpack";
        println ctx "_hx_table.maxn = _G.table.maxn or function(t)";
        println ctx "  local maxn=0;";
        println ctx "  for i in pairs(t) do";
        println ctx "    maxn=type(i)=='number'and i>maxn and i or maxn";
        println ctx "  end";
        println ctx "  return maxn";
        println ctx "end;";
    end;

    if has_feature ctx "use._hx_wrap_if_string_field" then begin
        println ctx "_hx_wrap_if_string_field = function(o, fld)";
        println ctx "  if _G.type(o) == 'string' then";
        println ctx "    if fld == 'length' then";
        println ctx "      return _G.string.len(o)";
        println ctx "    else";
        println ctx "      return String.prototype[fld]";
        println ctx "    end";
        println ctx "  else";
        println ctx "    return o[fld]";
        println ctx "  end";
        println ctx "end";
    end;

    if has_feature ctx "use._hx_dyn_add" then begin
        println ctx "_hx_dyn_add = function(a,b)";
        println ctx "  if (_G.type(a) == 'string' or _G.type(b) == 'string') then ";
        println ctx "    return Std.string(a)..Std.string(b)";
        println ctx "  else ";
        println ctx "    return a + b;";
        println ctx "  end;";
        println ctx "end;";
    end;

    println ctx "_hx_static_init();";

    List.iter (generate_enumMeta_fields ctx) com.types;

    (match com.main with
     | None -> ()
     | Some e -> gen_expr ctx e; newline ctx);

    if anyExposed then
        println ctx "return _hx_exports";

    let ch = open_out_bin com.file in
    output_string ch (Buffer.contents ctx.buf);
    close_out ch

