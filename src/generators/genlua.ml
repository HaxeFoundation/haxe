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

type pos = Ast.pos

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
	mutable iife_assign : bool;
	mutable handle_break : bool;
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

let get_exposed ctx path meta = try
		let (_, args, pos) = Meta.get Meta.Expose meta in
		(match args with
			| [ EConst (String s), _ ] -> [s]
			| [] -> [path]
			| _ -> error "Invalid @:expose parameters" pos)
	with Not_found -> []

let dot_path = Ast.s_type_path

let s_path ctx = dot_path

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

let anon_field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "['" ^ (Ast.s_escape s) ^ "']" else s
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

(* spr with newline *)
let sprln ctx s = spr ctx s; newline ctx

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

let rec iter_switch_break in_switch e =
	match e.eexpr with
	| TFunction _ | TWhile _ | TFor _ -> ()
	| TSwitch _ when not in_switch -> iter_switch_break true e
	| TBreak when in_switch -> raise Exit
	| _ -> iter (iter_switch_break in_switch) e

let handle_break ctx e =
    (* TODO: This got messy. Find a way to unify the implementation with a try/catch helper at least *)
	let old = ctx.in_loop, ctx.handle_break in
	ctx.in_loop <- true;
	try
		iter_switch_break false e;
		ctx.handle_break <- false;
		(fun() ->
			ctx.in_loop <- fst old;
			ctx.handle_break <- snd old;
		)
	with
		Exit ->
			sprln ctx "local _hx_expected_result = {}";
			sprln ctx "local _hx_status, _hx_result = pcall(function() ";
			let b = open_block ctx in
			newline ctx;
			ctx.handle_break <- true;
			(fun() ->
				b();
				ctx.in_loop <- fst old;
				ctx.handle_break <- snd old;
				newline ctx;
				sprln ctx "end";
				sprln ctx " return _hx_expected_result end)";
				spr ctx " if not _hx_status then ";
				newline ctx;
				spr ctx " elseif _hx_result ~= _hx_expected_result then return _hx_result";
			)

let this ctx = match ctx.in_value with None -> "self" | Some _ -> "self"

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

(* from genphp *)
let rec is_uncertain_type t =
	match follow t with
	| TInst (c, _) -> c.cl_interface
	| TMono _ -> true
	| TAnon a ->
	  (match !(a.a_status) with
	  | Statics _
	  | EnumStatics _ -> false
	  | _ -> true)
	| TDynamic _ -> true
	| _ -> false

let is_uncertain_expr e =
	is_uncertain_type e.etype

let rec is_anonym_type t =
	match follow t with
	| TAnon a ->
	  (match !(a.a_status) with
	  | Statics _
	  | EnumStatics _ -> false
	  | _ -> true)
	| TDynamic _ -> true
	| _ -> false

let is_anonym_expr e = is_anonym_type e.etype

let rec is_unknown_type t =
	match follow t with
	| TMono r ->
		(match !r with
		| None -> true
		| Some t -> is_unknown_type t)
	| _ -> false

let is_unknown_expr e =	is_unknown_type e.etype

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

let is_multireturn ctx e =
    match follow(e.etype) with
	| TInst(cl,_) ->  Meta.has Meta.MultiReturn cl.cl_meta
	| _ -> false

let multireturn_ordered_fields t =
    match follow(t) with
	| TInst (tc,_) -> tc.cl_ordered_fields
	| _ -> []

let rec find f lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if (f h) then 0 else 1 + (find f t)

let multireturn_idx e name =
    find (fun f -> f.cf_name == name) (multireturn_ordered_fields e.etype)


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

let rec should_wrap_int_op ctx op e1 e2 =
    match op with
    | Ast.OpAdd | Ast.OpMult | Ast.OpDiv | Ast.OpSub | Ast.OpAnd | Ast.OpOr
    | Ast.OpXor | Ast.OpShl  | Ast.OpShr | Ast.OpUShr ->
	    is_int_type ctx e1.etype && is_int_type ctx e2.etype
    | _ -> false

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s -> begin
	    add_feature ctx "use.string";
	    print ctx "\"%s\"" (Ast.s_escape s)
	end
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "nil"
	| TThis -> spr ctx (this ctx)
	| TSuper -> assert false

let rec gen_call ctx e el in_value =
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
	| TCall (x,_) , el when (match x.eexpr with TLocal { v_name = "__lua__" } -> false | _ -> true) ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TLocal { v_name = "__new__" }, { eexpr = TConst (TString cl) } :: params ->
		print ctx "%s.new(" cl;
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__new__" }, e :: params ->
		gen_value ctx e;
		spr ctx ".new(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__callself__" }, { eexpr = TConst (TString head) } :: { eexpr = TConst (TString tail) } :: el ->
		print ctx "%s:%s" head tail;
		spr ctx "(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")";
	| TLocal { v_name = "__call__" }, { eexpr = TConst (TString code) } :: el ->
		spr ctx code;
		spr ctx "(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")";
	| TLocal { v_name = "__lua_table__" }, el ->
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
			    concat ctx ", " (fun (f,e) ->
				print ctx "%s = " (anon_field f);
				gen_value ctx e
			    ) fields;
			    if List.length(fields) > 0 then incr count;
		    | { eexpr = TConst(TNull)} -> ()
		    | _ ->
			    error "__lua_table__ only accepts array or anonymous object arguments" e.epos;
		)) el;
		spr ctx "})";
	| TLocal { v_name = "__lua__" }, [{ eexpr = TConst (TString code) }] ->
		spr ctx (String.concat "\n" (ExtString.String.nsplit code "\r\n"))
	| TLocal { v_name = "__lua__" }, { eexpr = TConst (TString code); epos = p } :: tl ->
		Codegen.interpolate_code ctx.com code tl (spr ctx) (gen_expr ctx) p
	| TLocal { v_name = "__type__" },  [o] ->
		spr ctx "type(";
		gen_value ctx o;
		spr ctx ")";
	| TLocal ({v_name = "__define_feature__"}), [_;e] ->
		gen_expr ctx e
	| TLocal { v_name = "__feature__" }, { eexpr = TConst (TString f) } :: eif :: eelse ->
		(if has_feature ctx f then
			gen_value ctx eif
		else match eelse with
			| [] -> ()
			| e :: _ -> gen_value ctx e)
	| TLocal { v_name = "__resources__" }, [] ->
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
	| TLocal { v_name = "`trace" }, [e;infos] ->
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
		spr ctx ("(");
		gen_value ctx e;
		print ctx ("):%s(") (field_name ef);
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TField (e, ((FInstance _ | FAnon _ | FDynamic _) as ef)), el ->
		let s = (field_name ef) in
		if Hashtbl.mem kwds s || not (valid_lua_ident s) then begin
		    match e.eexpr with
		    |TNew _-> (
			spr ctx "_hx_apply_self(";
			gen_value ctx e;
			print ctx ",\"%s\"" (field_name ef);
			if List.length(el) > 0 then spr ctx ",";
			concat ctx "," (gen_value ctx) el;
			spr ctx ")";
		    );
		    |_ -> (
			gen_value ctx e;
			print ctx "%s(" (anon_field s);
			concat ctx "," (gen_value ctx) (e::el);
			spr ctx ")"
		    )
		end else begin
		    gen_value ctx e;
		    print ctx ":%s(" (field_name ef);
		    concat ctx "," (gen_value ctx) el;
		    spr ctx ")"
		end;
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")");
	ctx.iife_assign <- false;

and gen_mvar_name v f ctx =
    ctx.id_counter <- ctx.id_counter + 1;
    "_hx_" ^ (string_of_int ctx.id_counter) ^ "_" ^ v.v_name ^ "_" ^ f.cf_name


and gen_arg_name ctx (a,_) =
    match a.v_type with
    TInst (tc,_) when Meta.has Meta.MultiReturn tc.cl_meta->
	let mr_fields = multireturn_ordered_fields a.v_type in
	a.v_multi <- List.map (fun f -> gen_mvar_name a f ctx  ) mr_fields;
	a.v_name <- String.concat ", " a.v_multi;
	a.v_name;
    | _->
    	a.v_name

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
	| TField ({eexpr = TCall _} as e, f) when is_multireturn ctx e ->
		print ctx "_G.select(%i, " (multireturn_idx e (field_name f) + 1);
		gen_value ctx e;
		spr ctx ")";
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
	| TField (x, (FInstance(_,_,f) | FStatic(_,f) | FAnon(f))) when Meta.has Meta.SelfCall f.cf_meta ->
		gen_value ctx x;
	| TField ({ eexpr = TConst(TInt _ | TFloat _| TString _| TBool _) } as e , ((FInstance _ | FAnon _) as ef)) ->
		spr ctx ("(");
		gen_value ctx e;
		print ctx (").%s") (field_name ef);
	| TField ({ eexpr = TConst (TInt _ | TFloat _) } as x,f) ->
		gen_expr ctx { e with eexpr = TField(mk (TParenthesis x) x.etype x.epos,f) }
	| TField ({ eexpr = TObjectDecl fields }, ef ) ->
		spr ctx "(function(x) return x.";
		print ctx "%s" (field_name ef);
		spr ctx " end )({";
		concat ctx ", " (fun (f,e) -> print ctx "%s = " (anon_field f); gen_value ctx e) fields;
		spr ctx "})";
	| TField ({eexpr = TCall _; etype = TType (t,_)  } as e, f) when is_multireturn ctx e ->
		print ctx "_G.select(%i, " (multireturn_idx e (field_name f));
		gen_value ctx e;
		spr ctx ")";
	| TField ({eexpr = TLocal tv} as e, f) when is_multireturn ctx e ->
		(match f with
		| FInstance(_,_,cf)->
			let idx = multireturn_idx e cf.cf_name in
			spr ctx (List.nth tv.v_multi idx);
		| _-> ());
	| TField (e,f) ->
		gen_value ctx e;
		let name = field_name f in
		spr ctx (match f with FStatic _ | FEnum _ | FInstance _ | FAnon _ | FDynamic _ | FClosure _ -> field name)
	| TTypeExpr t ->
		spr ctx (ctx.type_accessor t)
	| TParenthesis e ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| TMeta (_,e) ->
		gen_expr ctx e
	| TReturn eo -> gen_return ctx e eo;
	| TBreak ->
		if not ctx.in_loop then unsupported e.epos;
		if ctx.handle_break then spr ctx "_G.error(\"_hx__break__\")" else spr ctx "break" (*todo*)
	| TContinue ->
		if not ctx.in_loop then unsupported e.epos;
		spr ctx "goto _hx_continue";
	| TBlock el ->
		let bend = open_block ctx in
		List.iter (gen_block_element ctx) el;
		bend();
		newline ctx;
	| TFunction f ->
		let old = ctx.in_value, ctx.in_loop in
		ctx.in_value <- None;
		ctx.in_loop <- false;
		print ctx "function(%s) " (String.concat "," (List.map ident (List.map (gen_arg_name ctx) f.tf_args)));
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
	| TCall (e2,e2args) ->
		    gen_call ctx e2 e2args false;
	| TArrayDecl el ->
		spr ctx "_hx_tab_array({";
		let count = ref 0 in
		List.iteri (fun i e ->
		    incr count;
		    if (i == 0) then spr ctx "[0]="
		    else spr ctx ", ";
		    gen_value ctx e) el;
		print ctx " }, %i)" !count;
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
				| TCall _ when is_multireturn ctx e ->
				    let mr_fields = multireturn_ordered_fields e.etype in
				    v.v_multi <- List.map (fun f -> gen_mvar_name v f ctx) mr_fields;
				    v.v_name <- String.concat ", " v.v_multi;
				    spr ctx "local ";
				    concat ctx ", " (spr ctx) v.v_multi;
				    spr ctx " = ";
				    gen_value ctx e;
				    semicolon ctx;
				| TBinop(OpAssign, e1, e2) ->
				    gen_tbinop ctx OpAssign e1 e2;
				    if local then
					spr ctx " local ";
				    spr ctx (ident v.v_name);
				    spr ctx " = ";
				    gen_value ctx e1;
				    semicolon ctx;

				| _ ->
				    if local then
					spr ctx "local ";
				    spr ctx (ident v.v_name);
				    spr ctx " = ";
				    gen_value ctx e;
				    semicolon ctx;
		end
	| TNew (c,_,el) ->
		(match c.cl_constructor with
		| Some cf when Meta.has Meta.SelfCall cf.cf_meta -> ()
		| _ -> print ctx "%s.new" (ctx.type_accessor (TClassDecl c)));
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
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
		sprln ctx "(function() ";
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
		let handle_break = handle_break ctx e in
		spr ctx "while ";
		gen_cond ctx cond;
		spr ctx " do ";
		let b = open_block ctx in
		gen_block_element ctx e;
		b();
		if has_continue e then begin
		    newline ctx;
		    spr ctx "::_hx_continue::";
		end;
		newline ctx;
		handle_break();
		newline ctx;
		spr ctx "end";
	| TWhile (cond,e,Ast.DoWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "while true do ";
		gen_block_element ctx e;
		newline ctx;
		spr ctx " while ";
		gen_cond ctx cond;
		spr ctx " do ";
		gen_block_element ctx e;
		handle_break();
		newline ctx;
		if has_continue e then begin
		    newline ctx;
		    spr ctx "::_hx_continue::";
		end;
		newline ctx;
		sprln ctx "end";
		spr ctx "break end";
	| TObjectDecl [] ->
		spr ctx "_hx_empty()";
		ctx.separator <- true
	| TObjectDecl fields ->
		spr ctx "_hx_o({__fields__={";
		concat ctx "," (fun (f,e) -> print ctx "%s=" (anon_field f); spr ctx "true") fields;
		spr ctx "},";
		concat ctx "," (fun (f,e) -> print ctx "%s=" (anon_field f); gen_value ctx e) fields;
		spr ctx "})";
		ctx.separator <- true
	| TFor (v,it,e) ->
		let handle_break = handle_break ctx e in
		let it = ident (match it.eexpr with
			| TLocal v -> v.v_name
			| _ ->
				let name = temp ctx in
				print ctx "local %s = " name;
				gen_value ctx it;
				newline ctx;
				name
		) in
		print ctx "while( %s:hasNext() ) do" it;
		let bend = open_block ctx in
		newline ctx;
		print ctx "local %s = %s:next();" (ident v.v_name) it;
		gen_block_element ctx e;
		bend();
		newline ctx;
		spr ctx "end";
		handle_break();
		newline ctx;
	| TTry (e,catchs) ->
		(* TODO: add temp variables *)
		sprln ctx "local _hx_expected_result = {}";
		spr ctx "local _hx_status, _hx_result = pcall(function() ";
		gen_expr ctx e;
		let vname = temp ctx in
		sprln ctx " return _hx_expected_result end)";
		spr ctx " if not _hx_status then ";
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
		spr ctx " elseif _hx_result ~= _hx_expected_result then return _hx_result end";
	| TSwitch (e,cases,def) ->
		List.iteri (fun cnt (el,e2) ->
		    if cnt == 0 then spr ctx "if " else spr ctx "elseif ";
		    List.iteri (fun ccnt e3 ->
			if ccnt > 0 then spr ctx " or ";
			gen_value ctx e;
			spr ctx " == ";
			gen_value ctx e3;
		    ) el;
		    spr ctx " then ";
		    let bend = open_block ctx in
		    gen_block_element ctx e2;
		    bend();
		    newline ctx;
		) cases;
		(match def with
		| None -> spr ctx "end"
		| Some e ->
			begin
			if (List.length(cases) > 0) then
			    spr ctx "else";
			let bend = open_block ctx in
			gen_block_element ctx e;
			bend();
			newline ctx;
			if (List.length(cases) > 0) then
			    spr ctx "end";
			end;);
	| TCast (e1,Some t) ->
		print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
		gen_expr ctx e1;
		spr ctx " , ";
		spr ctx (ctx.type_accessor t);
		spr ctx ")"
	| TCast (e1,None) ->
		gen_value ctx e1;
end;

and gen__init__hoist ctx e =
    begin match e.eexpr with
	| TVar (v,eo) ->(
		print ctx ", %s" (ident v.v_name);
	    )
	| TBlock el ->
		List.iter (gen__init__hoist ctx) el
	| TCall (e, el) ->
		(match e.eexpr , el with
		    | TLocal { v_name = "__feature__" }, { eexpr = TConst (TString f) } :: eif :: eelse ->
			    (if has_feature ctx f then
				    gen__init__hoist ctx eif
			    else match eelse with
				    | [] -> ()
				    | e :: _ -> gen__init__hoist ctx e)
		    |_->());
	| _ -> ()
    end;

and gen__init__impl ctx e =
    begin match e.eexpr with
	| TVar (v,eo) ->
		newline ctx;
		gen_expr ctx e
	| TBlock el ->
		List.iter (gen__init__impl ctx) el
	| TCall (e, el) ->
		(match e.eexpr , el with
		    | TLocal { v_name = "__feature__" }, { eexpr = TConst (TString f) } :: eif :: eelse ->
			    (if has_feature ctx f then
				    gen__init__impl ctx eif
			    else match eelse with
				    | [] -> ()
				    | e :: _ -> gen__init__impl ctx e)
		    |_->
			begin
			    newline ctx;
			    gen_call ctx e el false
			end;
			    );
	| _ -> gen_block_element ctx e;
    end;

and gen_block_element ?(after=false) ctx e  =
    newline ctx;
    ctx.iife_assign <- false;
    begin match e.eexpr with
	| TTypeExpr _ -> ()
	| TCast (ce,_) -> gen_block_element ctx ce
	| TParenthesis pe -> gen_block_element ctx pe
	| TArrayDecl el -> concat ctx " " (gen_block_element ctx) el;
	| TBinop (op,e1,e2) when op <> Ast.OpAssign ->
		let f () = gen_tbinop ctx op e1 e2 in
		gen_iife_assign ctx f;
		semicolon ctx;
	| TUnop ((Increment|Decrement) as op,_,e) ->
		newline ctx;
		gen_expr ctx e;
		print ctx " = ";
		gen_expr ctx e;
		(match op with
			| Increment -> print ctx " + 1;"
			| _ -> print ctx " - 1;"
		)
	| TArray (e1,e2) ->
		gen_block_element ctx e1;
		gen_block_element ctx e2;
	| TSwitch (e,[],def) ->
		(match def with
		| None -> ()
		| Some e -> gen_block_element ctx e)
	| TField _ ->
		let f () = gen_expr ctx e in
		gen_iife_assign ctx f;
		semicolon ctx;
	| TConst _ | TLocal _ -> ()
	| TBlock el ->
		List.iter (gen_block_element ~after ctx) el
	| TCall ({ eexpr = TLocal { v_name = "__feature__" } }, { eexpr = TConst (TString f) } :: eif :: eelse) ->
		if has_feature ctx f then
			gen_block_element ~after ctx eif
		else (match eelse with
			| [] -> ()
			| [e] -> gen_block_element ~after ctx e
			| _ -> assert false)
	| TFunction _ -> ()
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> gen_block_element ~after ctx e) fl
	| TVar (v,eo) ->
		gen_expr ctx e; (* these already generate semicolons*)
	| TMeta (_,e) ->
		gen_block_element ctx e
	| _ ->
		gen_expr ctx e;
		semicolon ctx;
		if after then newline ctx;
    end;

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
		sprln ctx ("local " ^ r_id);
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
	| TTypeExpr _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TNew _
	| TUnop _
	| TFunction _ ->
		gen_expr ctx e
	| TMeta (_,e1) ->
		gen_value ctx e1
	| TCall (e,el) ->
		gen_call ctx e el true
	| TReturn _
	| TBreak
	| TContinue ->
		unsupported e.epos
	(* TODO: this is just a hack because this specific case is a TestReflect unit test. I don't know how to address this properly
	   at the moment. - Simon *)
	| TCast ({ eexpr = TTypeExpr mt } as e1, None) when (match mt with TClassDecl {cl_path = ([],"Array")} -> false | _ -> true) ->
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
			gen_expr ctx (assign e2);
			semicolon ctx;
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
    match t with
    | TFun _ -> true
    | TMono r -> (match !r with | Some (TFun _) -> true | _ -> false)
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
	    | TField(e3, FInstance _ ), TField(e4, FClosure _ )  ->
		gen_value ctx e1;
		print ctx " %s " (Ast.s_binop op);
		spr ctx "_hx_functionToInstanceFunction(";
		gen_value ctx e2;
		spr ctx ")";
	    | TField(_, FInstance _ ), TLocal t  when (is_function_type ctx t.v_type)   ->
		gen_value ctx e1;
		print ctx " %s " (Ast.s_binop op);
		spr ctx "_hx_functionToInstanceFunction(";
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
	    sprln ctx "(function() ";
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
	    sprln ctx "(function() ";
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
    | Ast.OpAdd,_,_ when (is_string_expr e1 || is_string_expr e2) ->
	    gen_value ctx e1;
	    print ctx " .. ";
	    gen_value ctx e2;
    | _ -> begin
	    spr ctx "(";
	    gen_value ctx e1;
	    spr ctx ")";
	    (match op with
		| Ast.OpNotEq -> print ctx " ~= ";
		| Ast.OpBoolAnd -> print ctx " and ";
		| Ast.OpBoolOr -> print ctx " or ";
		| _ -> print ctx " %s " (Ast.s_binop op));
	    spr ctx "(";
	    gen_value ctx e2;
	    spr ctx ")";
	    end;
    );

and gen_wrap_tbinop ctx e=
    match e.eexpr with
    | TBinop _  ->
	    spr ctx "(";
	    gen_value ctx e;
	    spr ctx ")";
    | _ ->
	    gen_value ctx e

and gen_bitop ctx op e1 e2 =
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

and gen_cond ctx cond =
    ctx.iife_assign <- true;
    gen_value ctx cond;
    ctx.iife_assign <- false;

and has_class ctx c =
    has_feature ctx "lua.Boot.getClass" && (c.cl_super <> None || c.cl_ordered_fields <> [] || c.cl_constructor <> None)

and has_prototype ctx c =
    c.cl_super <> None || (has_class ctx c) || List.exists (can_gen_class_field ctx) c.cl_ordered_fields

and can_gen_class_field ctx = function
	| { cf_expr = (None | Some { eexpr = TConst TNull }) } when not (has_feature ctx "Type.getInstanceFields") ->
		false
	| f ->
		not (is_extern_field f)

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

let generate_package_create ctx (p,_) =
	let rec loop acc = function
		| [] -> ()
		| p :: l when Hashtbl.mem ctx.packages (p :: acc) -> loop (p :: acc) l
		| p :: l ->
			Hashtbl.add ctx.packages (p :: acc) ();
			(match acc with
			| [] -> print ctx "local %s = {}" p
			| _ ->
				let p = String.concat "." (List.rev acc) ^ (field p) in
				print ctx "%s = {}" p
			);
			ctx.separator <- true;
			newline ctx;
			loop (p :: acc) l
	in
	match p with
	| [] -> print ctx "local "
	| _ -> loop [] p

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
	| None when is_extern_field f ->
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

let gen_class_field ctx c f predelimit =
	check_field_name c f;
	if predelimit then sprln ctx ",";
	match f.cf_expr with
	| None ->
		print ctx "'%s', nil" f.cf_name;
	| Some e ->
		ctx.id_counter <- 0;
		(match e.eexpr with
		| TFunction f2 ->
		    let old = ctx.in_value, ctx.in_loop in
		    ctx.in_value <- None;
		    ctx.in_loop <- false;
		    print ctx "'%s', function" f.cf_name;
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
		    spr ctx "end";
		    ctx.in_value <- fst old;
		    ctx.in_loop <- snd old;
		    ctx.separator <- true;
		| _ -> gen_value ctx e)

let generate_class___name__ ctx c =
	if has_feature ctx "lua.Boot.isClass" then begin
		let p = s_path ctx c.cl_path in
		print ctx "%s.__name__ = " p;
		if has_feature ctx "Type.getClassName" then
			println ctx "{%s}" (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst c.cl_path @ [snd c.cl_path])))
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
					if p = "String" then sprln ctx "self = string";
					spr ctx "return self";
					bend(); newline ctx;
					spr ctx "end"; newline ctx; newline ctx;
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

	(match (get_exposed ctx (dot_path c.cl_path) c.cl_meta) with [s] -> (print ctx "_hx_exports%s = %s" (path_to_brackets s) p; newline ctx) | _ -> ());

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

	newline ctx;
	if (has_prototype ctx c) then begin
		print ctx "%s.prototype = _hx_anon(" p;
		let bend = open_block ctx in
		newline ctx;
		let count = ref 0 in

		List.iter (fun f -> if can_gen_class_field ctx f then (gen_class_field ctx c f (!count > 0); incr count;) ) c.cl_ordered_fields;
		if (has_class ctx c) then begin
			newprop ctx;
			if !count > 0 then spr ctx ",";
			print ctx "'__class__',  %s" p;
			incr count;
		end;

		if has_property_reflection then begin
			let props = Codegen.get_properties c.cl_ordered_fields in
			(match c.cl_super with
			| _ when props = [] -> ()
			| _ ->
				if !count > 0 then spr ctx ",";
				newprop ctx;
				print ctx "'__properties__',  {%s}" (gen_props props));
		end;

		bend(); newline ctx;
		println ctx ")";
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
	let ename = List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst e.e_path @ [snd e.e_path]) in

	(* TODO: Unify the _hxClasses declaration *)
	if has_feature ctx "Type.resolveEnum" then begin
	    newline ctx;
	    print ctx "_hxClasses[\"%s\"] = %s" (dot_path e.e_path) p; semicolon ctx; newline ctx;
	end;
	if has_feature ctx "lua.Boot.isEnum" then begin
	    newline ctx;
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
				spr ctx " _x.toString = _estr;";
			spr ctx " return _x; end ";
			ctx.separator <- true;
		| _ ->
			println ctx "_hx_tab_array({[0]=\"%s\",%d,__enum__ = %s},2)" f.ef_name f.ef_index p;
			if has_feature ctx "may_print_enum" then begin
				println ctx "%s%s.toString = _estr" p (field f.ef_name);
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

	generate_package_create ctx path;

	(match args with
	| [(EConst(String(module_name)),_)] ->
		print ctx "%s = _G.require(\"%s\")" p module_name
	| [(EConst(String(module_name)),_) ; (EConst(String(object_path)),_)] ->
		print ctx "%s = _G.require(\"%s\").%s" p module_name object_path
	| _ ->
		error "Unsupported @:luaRequire format" mp);

	newline ctx


let check_multireturn ctx c =
    match c with
    | _ when Meta.has Meta.MultiReturn c.cl_meta ->
	    if not c.cl_extern then
		error "MultiReturns must be externs" c.cl_pos
	    else if (match c.cl_kind with KExtension _ -> true | _ -> false) then
		error "MultiReturns must not extend another class" c.cl_pos
	    else if List.length c.cl_ordered_statics > 0 then
		error "MultiReturns must not contain static fields" c.cl_pos
	    else if (List.exists (fun cf -> match cf.cf_kind with Method _ -> true | _-> false) c.cl_ordered_fields) then
		error "MultiReturns must not contain methods" c.cl_pos;
    | {cl_super = Some(csup,_)} when Meta.has Meta.MultiReturn csup.cl_meta ->
		error "Cannot extend a MultiReturn" c.cl_pos
    | _ -> ()

let generate_type ctx = function
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e ->
			ctx.inits <- e :: ctx.inits);
		(* Special case, want to add Math.__name__ only when required, handle here since Math is extern *)
		let p = s_path ctx c.cl_path in
		if p = "Math" then generate_class___name__ ctx c;
		(* Another special case for Std because we do not want to generate it if it's empty. *)
		if p = "Std" && c.cl_ordered_statics = [] then
			()
		else if not c.cl_extern then
			generate_class ctx c
		else if Meta.has Meta.LuaRequire c.cl_meta && is_directly_used ctx.com c.cl_meta then
			generate_require ctx c.cl_path c.cl_meta
		else if Meta.has Meta.InitPackage c.cl_meta then
			(match c.cl_path with
			| ([],_) -> ()
			| _ -> generate_package_create ctx c.cl_path);

		check_multireturn ctx c;
	| TEnumDecl e when e.e_extern ->
		if Meta.has Meta.LuaRequire e.e_meta && is_directly_used ctx.com e.e_meta then
		    generate_require ctx e.e_path e.e_meta;
	| TEnumDecl e -> generate_enum ctx e
	| TTypeDecl _ | TAbstractDecl _ -> ()

let generate_type_forward ctx = function
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e ->
			ctx.inits <- e :: ctx.inits);
		if not c.cl_extern then begin
		    generate_package_create ctx c.cl_path;
		    let p = s_path ctx c.cl_path in
		    println ctx "%s = _hx_empty()" p;
		end
	| TEnumDecl e when e.e_extern ->
		()
	| TEnumDecl e ->
		generate_package_create ctx e.e_path;
		let p = s_path ctx e.e_path in
		println ctx "%s = _hx_empty()" p;
	| TTypeDecl _ | TAbstractDecl _ -> ()

let set_current_class ctx c =
	ctx.current <- c

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
		handle_break = false;
		id_counter = 0;
		type_accessor = (fun _ -> assert false);
		separator = false;
		found_expose = false;
		lua_jit = Common.defined com Define.LuaJit;
		lua_ver = try
			float_of_string (PMap.find "lua_ver" com.defines)
		    with | Not_found -> 5.2;
	} in
	ctx.type_accessor <- (fun t ->
		let p = t_path t in
		match t with
		| TClassDecl ({ cl_extern = true } as c) when not (Meta.has Meta.LuaRequire c.cl_meta)
			-> dot_path p
		| TEnumDecl { e_extern = true }
			-> dot_path p
		| _ -> s_path ctx p);
	ctx

let gen_single_expr ctx e expr =
	if expr then gen_expr ctx e else gen_value ctx e;
	let str = Buffer.contents ctx.buf in
	Buffer.reset ctx.buf;
	ctx.id_counter <- 0;
	str

let generate com =
	let t = Common.timer "generate lua" in
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
		print ctx "%s = %s or _hx_empty()" path path;
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

	spr ctx "local _hx_bind";
	List.iter (gen__init__hoist ctx) (List.rev ctx.inits); newline ctx;
	ctx.inits <- []; (* reset inits *)

	List.iter (generate_type ctx) com.types;

	(* If we use haxe Strings, patch Lua's string *)
	if has_feature ctx "use.string" then begin
	    sprln ctx "local _hx_string_mt = _G.getmetatable('');";
	    sprln ctx "String.__oldindex = _hx_string_mt.__index;";
	    sprln ctx "_hx_string_mt.__index = String.__index;";
	    sprln ctx "_hx_string_mt.__add = function(a,b) return Std.string(a)..Std.string(b) end;";
	    sprln ctx "_hx_string_mt.__concat = _hx_string_mt.__add";
	end;

	(* Array is required, always patch it *)
	sprln ctx "_hx_array_mt.__index = Array.prototype";
	newline ctx;

	(* Generate statics *)
	List.iter (generate_static ctx) (List.rev ctx.statics);

	(* Localize init variables inside a do-block *)
	(* Note: __init__ logic can modify static variables. *)
	sprln ctx "do";
	List.iter (gen__init__impl ctx) (List.rev ctx.inits);
	newline ctx;
	sprln ctx "end";

	let rec chk_features e =
		if is_dynamic_iterator ctx e then add_feature ctx "use._iterator";
		match e.eexpr with
		| TField (_,FClosure _) ->
			add_feature ctx "use._hx_bind"
		| _ ->
			Type.iter chk_features e
	in

	List.iter chk_features ctx.inits;

	List.iter (fun (_,_,e) -> chk_features e) ctx.statics;
	if has_feature ctx "use._iterator" then begin
		add_feature ctx "use._hx_bind";
		println ctx "function _hx_iterator(o)  if ( lua.Boot.__instanceof(o, Array) ) then return function() return HxOverrides.iter(o) end elseif (typeof(o.iterator) == 'function') then return  _hx_bind(o,o.iterator) else return  o.iterator end end";
	end;
	if has_feature ctx "use._hx_bind" then println ctx "_hx_bind = lua.Boot.bind";

	List.iter (generate_enumMeta_fields ctx) com.types;

	(match com.main with
	| None -> ()
	| Some e -> gen_expr ctx e; newline ctx);

	sprln ctx "return _hx_exports";

	let ch = open_out_bin com.file in
	output_string ch (Buffer.contents ctx.buf);
	close_out ch;
	t()

