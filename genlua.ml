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
}

type object_store = {
	os_name : string;
	mutable os_fields : object_store list;
}

let get_exposed ctx path meta = try
		let (_, args, pos) = Meta.get Meta.Expose meta in
		(match args with
			| [ EConst (String s), _ ] -> [s]
			| [] -> [path]
			| _ -> error "Invalid @:expose parameters" pos)
	with Not_found -> []

let dot_path = Ast.s_type_path

let s_path ctx = dot_path

let debug_expression expression  =
    " --[[ " ^ Type.s_expr_kind expression  ^ " --]] "

let debug_type t  =
    " --[[ " ^ Type.s_type_kind t  ^ " --]] ";;

(* TODO: are all these kwds necessary for field quotes *and* id escapes? *)
let kwds =
	let h = Hashtbl.create 0 in
	List.iter (fun s -> Hashtbl.add h s ()) [
	    ""; "and"; "break"; "do"; "else"; "elseif";
	    "end"; "false"; "for"; "function"; "if";
	    "in"; "local"; "nil"; "not"; "or"; "repeat";
	    "return"; "then"; "true"; "until"; "while";
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

let anon_field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "['" ^ s ^ "']" else s
let static_field s =
    match s with
	| "length" | "name" -> "._" ^ s
	| s -> field s

let has_feature ctx = Common.has_feature ctx.com
let add_feature ctx = Common.add_feature ctx.com

let spr ctx s =
	ctx.separator <- false;
	Buffer.add_string ctx.buf s


let print ctx =
	ctx.separator <- false;
	Printf.kprintf (fun s -> begin
		Buffer.add_string ctx.buf s
	end)

let unsupported p = error "This expression cannot be compiled to Lua" p

let basename path =
	try
		let idx = String.rindex path '/' in
		String.sub path (idx + 1) (String.length path - idx - 1)
	with Not_found -> path


let newline ctx = print ctx "\n%s" ctx.tabs

(* TODO : make this work properly... it was inserting commas where they shouldn't be *)
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
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let rec iter_switch_break in_switch e =
	match e.eexpr with
	| TFunction _ | TWhile _ | TFor _ -> ()
	| TSwitch _ when not in_switch -> iter_switch_break true e
	| TBreak when in_switch -> raise Exit
	| _ -> iter (iter_switch_break in_switch) e

let handle_break ctx e =
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
			spr ctx "try {";
			let b = open_block ctx in
			newline ctx;
			ctx.handle_break <- true;
			(fun() ->
				b();
				ctx.in_loop <- fst old;
				ctx.handle_break <- snd old;
				newline ctx;
				spr ctx "} catch( e ) { if( e != \"__break__\" ) throw e; }";
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
	| TString s -> print ctx "\"%s\"" (Ast.s_escape s)
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
			(* TODO: use nonconflict var instead of prototype *)
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
		spr ctx "{";
		concat ctx ", " (gen_value ctx) el;
		spr ctx "}";
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
		spr ctx "__tabArray({";
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
			spr ctx "print(";
			gen_value ctx e;
			spr ctx ")";
		end
	| TCall ({eexpr = TField(e,((FInstance _ | FAnon _) as ef)) }, _), el ->
		gen_value ctx e;
		print ctx ":%s(" (field_name ef);
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TField ( { eexpr = TConst(TInt _ | TFloat _| TString _| TBool _) } as e , ((FInstance _ | FAnon _) as ef)), el ->
		(* TODO: Come up with better workaround for dealing with invoked methods on constants e.g. "foo".charAt(0); *)
		spr ctx "(function(x) return x:";
		print ctx "%s" (field_name ef);
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ") end )(";
		gen_value ctx e;
		spr ctx ")";
	| TField (e, ((FInstance _ | FAnon _ | FDynamic _) as ef)), el ->
		gen_value ctx e;
		spr ctx ":";
		print ctx "%s" (field_name ef);
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")");
	ctx.iife_assign <- false;

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
	| TField (x,FClosure (Some ({cl_path=[],"Array"},_), {cf_name="push"})) ->
		(* see https://github.com/HaxeFoundation/haxe/issues/1997 *)
		add_feature ctx "use._arrayPushClosure";
		print ctx "_arrayPushClosure(";
		gen_value ctx x;
		print ctx ")"
	| TField (x,FClosure (_,f)) ->
		add_feature ctx "use._bind";
		(match x.eexpr with
		| TConst _ | TLocal _ ->
			print ctx "_bind(";
			gen_value ctx x;
			print ctx ",";
			gen_value ctx x;
			print ctx "%s)" (field f.cf_name)
		| _ ->
			print ctx "(__=";
			gen_value ctx x;
			print ctx ",_bind(__,__%s))" (field f.cf_name))
	| TEnumParameter (x,_,i) ->
		gen_value ctx x;
		print ctx "[%i]" (i + 2)
	| TField ({ eexpr = TConst(TInt _ | TFloat _| TString _| TBool _) } as e , ((FInstance _ | FAnon _) as ef)) ->
		spr ctx "(function(x) return x.";
		print ctx "%s" (field_name ef);
		spr ctx " end )(";
		gen_value ctx e;
		spr ctx ")";
	| TField ({ eexpr = TConst (TInt _ | TFloat _) } as x,f) ->
		gen_expr ctx { e with eexpr = TField(mk (TParenthesis x) x.etype x.epos,f) }
	| TField ({ eexpr = TObjectDecl fields }, ef ) ->
		spr ctx "(function(x) return x.";
		print ctx "%s" (field_name ef);
		spr ctx " end )({";
		concat ctx ", " (fun (f,e) -> print ctx "%s = " (anon_field f); gen_value ctx e) fields;
		spr ctx "})";
	| TField (x, (FInstance(_,_,f) | FStatic(_,f) | FAnon(f))) when Meta.has Meta.SelfCall f.cf_meta ->
		gen_value ctx x;
		(* TODO: Come up with better workaround for dealing with invoked methods on constants e.g. "foo".charAt(0); *)
	| TField (x,f) ->
		gen_value ctx x;
		let name = field_name f in
		spr ctx (match f with FStatic _ -> static_field name | FEnum _ | FInstance _ | FAnon _ | FDynamic _ | FClosure _ -> field name)
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
		if ctx.handle_break then spr ctx "throw \"__break__\"" else spr ctx "break"
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
		begin
		    gen_call ctx e el false;
		end;
	| TArrayDecl el ->
		spr ctx "__tabArray({";
		let count = ref 0 in
		List.iteri (fun i e ->
		    incr count;
		    if (i == 0) then spr ctx "[0]="
		    else spr ctx ", ";
		    gen_value ctx e) el;
		print ctx " }, %i)" !count;
	| TThrow e ->
		spr ctx "error(";
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
		print ctx "%s.new(" (ctx.type_accessor (TClassDecl c));
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
		spr ctx "(function() "; newline ctx;
		(match e.eexpr, unop_flag with
		    | TArray(e1,e2), _ ->
			spr ctx "local _idx = "; gen_value ctx e2; semicolon ctx; newline ctx;
			spr ctx "local _arr ="; gen_value ctx e1; semicolon ctx; newline ctx;
			(match unop_flag with
			    | Ast.Postfix ->
				    spr ctx "local _ = _arr[_idx]"; semicolon ctx; newline ctx;
			    | _ -> ());
			spr ctx "_arr[_idx] = _arr[_idx]";
		    | TField(e1,e2), _ ->
			spr ctx "local _obj = "; gen_value ctx e1; semicolon ctx; newline ctx;
			spr ctx "local _fld = ";
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
				    spr ctx "local _ = _obj[_fld]"; semicolon ctx; newline ctx;
			    | _ -> ());
			spr ctx "_obj[_fld] = _obj[_fld] ";
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
			    spr ctx "_arr[_idx]";
		    | _, TField(e1,e2) ->
			    spr ctx "_obj[_fld]";
		    | _, _ ->
			    gen_value ctx e;
		    );
		semicolon ctx; newline ctx;
		spr ctx " end)()";
	| TUnop (Not,unop_flag,e) ->
		spr ctx "not ";
		gen_value ctx e;
	| TUnop (NegBits,unop_flag,e) ->
		spr ctx "_G.bit.bnot(";
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
		gen_block_element ctx e;
		handle_break();
		if has_continue e then begin
		    newline ctx;
		    spr ctx "::_hx_continue::";
		end;
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
		spr ctx "end"; newline ctx;
		spr ctx "break end";
	| TObjectDecl fields ->
		spr ctx "_G.__anon(";
		concat ctx ", " (fun (f,e) -> print ctx "\"%s\", " f; gen_value ctx e) fields;
		spr ctx ")";
		ctx.separator <- true
	| TFor (v,it,e) ->
		let handle_break = handle_break ctx e in
		let it = ident (match it.eexpr with
			| TLocal v -> v.v_name
			| _ ->
				let id = ctx.id_counter in
				ctx.id_counter <- ctx.id_counter + 1;
				let name = "_it" ^ string_of_int id in
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
		spr ctx "local _expected_result = {}";
		newline ctx;
		spr ctx "local _status, _result = pcall(function() ";
		gen_expr ctx e;
		let vname =
			let id = ctx.id_counter in
			ctx.id_counter <- ctx.id_counter + 1;
			(* TODO : More temp var cleanup *)
			"_e" ^ string_of_int id
		in
		spr ctx " return _expected_result end)"; newline ctx;
		spr ctx " if not _status then ";
		let bend = open_block ctx in
		newline ctx;
		print ctx "local %s = _result" vname;
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
		    print ctx " error(%s)" vname;
		    newline ctx;
		    spr ctx "end";
		end;
		bend();
		newline ctx;
		spr ctx " elseif _result ~= _expected_result then return _result end";
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
	| TVar (v,eo) ->
		print ctx "local %s = {};" (ident v.v_name);
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
    end

and gen__init__impl ctx e =
    begin match e.eexpr with
	| TVar (v,eo) ->
		newline ctx;
		gen_expr ~local:false ctx e
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
    end

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
		let r_id = ctx.id_counter in
		ctx.id_counter <- ctx.id_counter + 1;
		let r = alloc_var ("r" ^ string_of_int r_id) t_dynamic in
		ctx.in_value <- Some r;
		ctx.in_loop <- false;
		spr ctx "(function() ";
		let b = open_block ctx in
		newline ctx;
		spr ctx ("local r" ^ string_of_int r_id);
		newline ctx;
		(fun() ->
			newline ctx;
			spr ctx ("return r" ^ string_of_int r_id);
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

and gen_assign_value ctx e =
    begin
	spr ctx (debug_expression e);
	match e.eexpr with
	| TCast ({ eexpr = TTypeExpr _ }, None) ->
		begin
		    spr ctx "_G.__staticToInstance(";
		    gen_value ctx e;
		    spr ctx ")";
		end
	| _ ->
		gen_value ctx e;
    end;

and gen_tbinop ctx op e1 e2 =
    (match op, e1.eexpr, e2.eexpr with
    | Ast.OpAssign, TField(e3, FInstance(_,_,_) ), TFunction f ->
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
	    (match e2.eexpr with
	    | TBinop(OpAssign as op, e3, e4) ->
		gen_tbinop ctx op e3 e4;
		newline ctx;
		gen_value ctx e1;
		spr ctx " = ";
		gen_value ctx e3;
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
	    spr ctx "(function() "; newline ctx;
	    let idx = alloc_var "idx" e4.etype in
	    let idx_var =  mk (TVar( idx , Some(e4))) e4.etype e4.epos in
	    gen_expr ctx idx_var;
	    let arr = alloc_var "arr" e3.etype in
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
	    spr ctx "(function() "; newline ctx;
	    let obj = alloc_var "obj" e3.etype in
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
    print ctx "_G.bit.%s(" (match op with
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

let gen_class_static_field ctx c f =
	match f.cf_expr with
	| None | Some { eexpr = TConst TNull } when not (has_feature ctx "Type.getClassFields") ->
		()
	| None when is_extern_field f ->
		()
	| None ->
		print ctx "%s%s = nil" (s_path ctx c.cl_path) (static_field f.cf_name);
		newline ctx
	| Some e ->
		match e.eexpr with
		| TFunction _ ->
			let path = (s_path ctx c.cl_path) ^ (static_field f.cf_name) in
			ctx.id_counter <- 0;
			print ctx "%s = " path;
			gen_value ctx e;
			newline ctx;
		| _ ->
			ctx.statics <- (c,f.cf_name,e) :: ctx.statics

let gen_class_field ctx c f predelimit =
	check_field_name c f;
	if predelimit then (spr ctx ","; newline ctx;);
	match f.cf_expr with
	| None ->
		print ctx "'%s', nil" (anon_field f.cf_name);
	| Some e ->
		ctx.id_counter <- 0;
		(match e.eexpr with
		| TFunction f2 ->
		    let old = ctx.in_value, ctx.in_loop in
		    ctx.in_value <- None;
		    ctx.in_loop <- false;
		    print ctx "'%s', function" (anon_field f.cf_name);
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
			print ctx "{%s}" (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst c.cl_path @ [snd c.cl_path])))
		else
			print ctx "true";
		newline ctx;
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
					print ctx "local self = __anon();";
					newline ctx;
					if (has_prototype ctx c) then (
					    print ctx "getmetatable(self).__index=%s.prototype" p; newline ctx;
					);
					print ctx "%s.super(%s)" p (String.concat "," ("self" :: (List.map ident (List.map arg_name f.tf_args))));
					newline ctx;
					if p = "String" then begin
					    spr ctx "self = string";
					    newline ctx;
					end;
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
	if hxClasses then begin
		(* TODO: better namespace for _hxClasses *)
		print ctx "_hxClasses[\"%s\"] = %s" (dot_path c.cl_path) p;
		newline ctx;
	end;
	generate_class___name__ ctx c;
	(match c.cl_implements with
	| [] -> ()
	| l ->
		print ctx "%s.__interfaces__ = {%s}" p (String.concat "," (List.map (fun (i,_) -> ctx.type_accessor (TClassDecl i)) l));
		newline ctx;
	);

	let gen_props props =
		String.concat "," (List.map (fun (p,v) -> p ^"=\""^v^"\"") props) in
	let has_property_reflection =
		(has_feature ctx "Reflect.getProperty") || (has_feature ctx "Reflect.setProperty") in

	if has_property_reflection then begin
		(match Codegen.get_properties c.cl_ordered_statics with
		| [] -> ()
		| props ->
			print ctx "%s.__properties__ = {%s}" p (gen_props props);
			newline ctx);
	end;

	List.iter (gen_class_static_field ctx c) c.cl_ordered_statics;

	newline ctx;
	if (has_prototype ctx c) then begin
		print ctx "%s.prototype = _G.__anon(" p;
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
		print ctx ")";
		newline ctx;
		(match c.cl_super with
		| None -> ()
		| Some (csup,_) ->
			let psup = ctx.type_accessor (TClassDecl csup) in
			print ctx "%s.__super__ = %s" p psup; newline ctx;
			print ctx "setmetatable(%s.prototype,{__index=%s.prototype})" p psup; newline ctx;
			if has_property_reflection && Codegen.has_properties csup then begin
			    (* Also use the __properties__  from the super class as the __index metatable *)
			    print ctx "setmetatable(%s.prototype.__properties__,{__index=%s.prototype.__properties__})" p psup; newline ctx;
			end;
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
	    (* TODO :  Come up with a helper function for __tabArray declarations *)
	    spr ctx " __constructs__ = __tabArray({";
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
			print ctx "function(%s)  local _x = __tabArray({[0]=\"%s\",%d,%s,__enum__=%s}, %i);" sargs f.ef_name f.ef_index sargs p (count + 2);
			if has_feature ctx "may_print_enum" then
				(* TODO: better namespacing for _estr *)
				spr ctx " _x.toString = _estr;";
			spr ctx " return _x; end ";
			ctx.separator <- true;
		| _ ->
			print ctx "__tabArray({[0]=\"%s\",%d},2)" f.ef_name f.ef_index;
			newline ctx;
			if has_feature ctx "may_print_enum" then begin
				print ctx "%s%s.toString = _estr" p (field f.ef_name);
				newline ctx;
			end;
			print ctx "%s%s.__enum__ = %s" p (field f.ef_name) p;
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
		spr ctx "__tabArray({";
		if (List.length ctors_without_args)  > 0 then
		    begin
			spr ctx "[0] = ";
			print ctx "%s" (String.concat "," (List.map (fun s -> Printf.sprintf "%s.%s" p s) ctors_without_args));
		    end;
		print ctx "}, %i)"  (List.length ctors_without_args);
		newline ctx
	end

let generate_static ctx (c,f,e) =
	print ctx "%s%s = " (s_path ctx c.cl_path) (static_field f);
	gen_assign_value ctx e;
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

let generate_require ctx c =
	let _, args, mp = Meta.get Meta.LuaRequire c.cl_meta in
	let p = (s_path ctx c.cl_path) in

	generate_package_create ctx c.cl_path;

	(match args with
	| [(EConst(String(module_name)),_)] ->
		print ctx "%s = require(\"%s\")" p module_name
	| [(EConst(String(module_name)),_) ; (EConst(String(object_path)),_)] ->
		print ctx "%s = require(\"%s\").%s" p module_name object_path
	| _ ->
		error "Unsupported @:luaRequire format" mp);

	newline ctx

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
		else if (Meta.has Meta.LuaRequire c.cl_meta) then
			generate_require ctx c
		else if Meta.has Meta.InitPackage c.cl_meta then
			(match c.cl_path with
			| ([],_) -> ()
			| _ -> generate_package_create ctx c.cl_path)
	| TEnumDecl e when e.e_extern ->
		()
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
		    print ctx "%s = __anon() " p;
		end
	| TEnumDecl e when e.e_extern ->
		()
	| TEnumDecl e ->
		generate_package_create ctx e.e_path;
		let p = s_path ctx e.e_path in
		print ctx "%s = __anon() " p;
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

	if has_feature ctx "Class" || has_feature ctx "Type.getClassName" then add_feature ctx "lua.Boot.isClass";
	if has_feature ctx "Enum" || has_feature ctx "Type.getEnumName" then add_feature ctx "lua.Boot.isEnum";

	spr ctx "pcall(require, 'bit32') pcall(require, 'bit') bit = bit or bit32"; newline ctx;
	spr ctx "print = print or (function()end)"; newline ctx;

	spr ctx "__anon = function(...)"; newline ctx;
	spr ctx "   local ret = {__fields__ = {}};"; newline ctx;
	spr ctx "   local max = select('#',...);"; newline ctx;
	spr ctx "   local tab = {...};"; newline ctx;
	spr ctx "   local cur = 1;"; newline ctx;
	spr ctx "   while cur < max do"; newline ctx;
	spr ctx "	local v = tab[cur];"; newline ctx;
	spr ctx "	ret.__fields__[v] = true;"; newline ctx;
	spr ctx "	ret[v] = tab[cur+1];"; newline ctx;
	spr ctx "	cur = cur + 2"; newline ctx;
	spr ctx "   end"; newline ctx;
	spr ctx "   setmetatable(ret, {__newindex=function(t,k,v) t.__fields__[k] = true; rawset(t,k,v); end})"; newline ctx;
	spr ctx "   return ret; "; newline ctx;
	spr ctx "end"; newline ctx;

	spr ctx "__staticToInstance = function(tab)"; newline ctx;
	spr ctx "   return _G.setmetatable({}, {"; newline ctx;
	spr ctx "	__index = function(t,k)"; newline ctx;
	spr ctx "	    if _G.type(rawget(tab,k)) == 'function' then "; newline ctx;
	spr ctx "		return function(self,...)"; newline ctx;
	spr ctx "		    return rawget(tab,k)(...)"; newline ctx;
	spr ctx "		end"; newline ctx;
	spr ctx "	    else"; newline ctx;
	spr ctx "		return rawget(tab,k)"; newline ctx;
	spr ctx "	    end"; newline ctx;
	spr ctx "	end"; newline ctx;
	spr ctx "   })"; newline ctx;
	spr ctx "end"; newline ctx;


	spr ctx "_hxClasses = {}"; semicolon ctx; newline ctx;
	let vars = [] in
	(* let vars = (if has_feature ctx "Type.resolveClass" || has_feature ctx "Type.resolveEnum" then ("_hxClasses = " ^ "{}") :: vars else vars) in *)
	let vars = if has_feature ctx "may_print_enum"
		then ("_estr = function(self)  return " ^ (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" })) ^ ".__string_rec(self,''); end") :: vars
		else vars in
	(match List.rev vars with
	| [] -> ()
	| vl ->
		print ctx "local %s" (String.concat ";" vl);
		ctx.separator <- true;
		newline ctx
	);

	spr ctx "--[[begin class hoists--]]"; newline ctx;
	List.iter (generate_type_forward ctx) com.types;
	newline ctx;
	spr ctx "--[[end class hoists--]]"; newline ctx;

	spr ctx "__tabArray = function(tab,length)"; newline ctx;
	spr ctx "   tab.length = length"; newline ctx;
	spr ctx "   setmetatable(tab, {"; newline ctx;
	spr ctx "	__index = Array.prototype,"; newline ctx;
	spr ctx "	__newindex = function(t,k,v)"; newline ctx;
	spr ctx "	    if _G.type(k) == 'number' and k >= t.length then"; newline ctx;
	spr ctx "		t.length = k + 1"; newline ctx;
	spr ctx "	    end"; newline ctx;
	spr ctx "	    rawset(t,k,v)"; newline ctx;
	spr ctx "	end"; newline ctx;
	spr ctx "   })"; newline ctx;
	spr ctx "   return tab"; newline ctx;
	spr ctx "end"; newline ctx;

	spr ctx "--[[begin __init__ hoist --]]"; newline ctx;
	List.iter (gen__init__hoist ctx) (List.rev ctx.inits);
	ctx.inits <- []; (* reset inits *)
	newline ctx;
	spr ctx "--[[end __init__ hoist --]]"; newline ctx;

	spr ctx "local _bind = {}";
	newline ctx;

	List.iter (generate_type ctx) com.types;
	let rec chk_features e =
		if is_dynamic_iterator ctx e then add_feature ctx "use._iterator";
		match e.eexpr with
		| TField (_,FClosure _) ->
			add_feature ctx "use._bind"
		| _ ->
			Type.iter chk_features e
	in
	List.iter chk_features ctx.inits;
	List.iter (fun (_,_,e) -> chk_features e) ctx.statics;
	if has_feature ctx "use._iterator" then begin
		add_feature ctx "use._bind";
		print ctx "function _iterator(o) { if ( lua.Boot.__instanceof(o, Array) ) return function() { return HxOverrides.iter(o); }; return typeof(o.iterator) == 'function' ? _bind(o,o.iterator) : o.iterator; }";
		newline ctx;
	end;
	if has_feature ctx "use._bind" then begin
		print ctx "_bind = lua.Boot.bind";
		newline ctx;
	end;
	if has_feature ctx "use._arrayPushClosure" then begin
		print ctx "function _arrayPushClosure(a) ";
		print ctx " return function(x) a:push(x); end; ";
		print ctx "end";
		newline ctx
	end;

	spr ctx "--[[ begin __init__impl --]]"; newline ctx;
	List.iter (gen__init__impl ctx) (List.rev ctx.inits);
	spr ctx "--[[ end __init__impl --]]"; newline ctx;

	spr ctx "--[[ begin __enumMeta__fields --]]"; newline ctx;
	List.iter (generate_enumMeta_fields ctx) com.types;
	spr ctx "--[[ end __enumMeta__fields --]]"; newline ctx;

	spr ctx "--[[ begin static fields --]]"; newline ctx;
	List.iter (generate_static ctx) (List.rev ctx.statics);
	spr ctx "--[[ end static fields --]]"; newline ctx;

	(match com.main with
	| None -> ()
	| Some e -> gen_expr ctx e; newline ctx);
	let ch = open_out_bin com.file in
	output_string ch (Buffer.contents ctx.buf);
	close_out ch;
	t()

