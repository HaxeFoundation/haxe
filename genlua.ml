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
	mutable handle_break : bool;
	mutable id_counter : int;
	mutable continue_counter : int;
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

let kwds =
	let h = Hashtbl.create 0 in
	List.iter (fun s -> Hashtbl.add h s ()) [
		"abstract"; "as"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "continue"; "const";
		"debugger"; "default"; "delete"; "do"; "double"; "else"; "enum"; "export"; "extends"; "false"; "final";
		"finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int";
		"interface"; "is"; "let"; "long"; "namespace"; "native"; "new"; "nil"; "package"; "private"; "protected";
		"public"; "return"; "short"; "static"; "super"; "switch"; "synchronized"; "this"; "throw"; "throws";
		"transient"; "true"; "try"; "typeof"; "use"; "var"; "void"; "volatile"; "while"; "with"; "yield";
		(* Lua keywords *)
		"and"; "elseif"; "end"; "local"; "not"; "or"; "repeat"; "then"; "until"; "_G"; "self"
	];
	h

let valid_lua_ident s =
	try
		for i = 0 to String.length s - 1 do
			match String.unsafe_get s i with
			| 'a'..'z' | 'A'..'Z' | '$' | '_' -> ()
			| '0'..'9' when i > 0 -> ()
			| _ -> raise Exit
		done;
		true
	with Exit ->
		false

let field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "[\"" ^ s ^ "\"]" else "." ^ s
let ident s = if Hashtbl.mem kwds s then "_" ^ s else s

let anon_field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "'" ^ s ^ "'" else s
let static_field s =
	match s with
	| "length" | "name" -> ".$" ^ s
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

let unsupported p = error "This expression cannot be compiled to Javascript" p

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

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s -> print ctx "\"%s\"" (Ast.s_escape s)
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "nil"
	| TThis -> spr ctx (this ctx)
	| TSuper -> assert false

let rec gen_call ctx e el in_value =
	(match e.eexpr , el with
	| TConst TSuper , params ->
		(match ctx.current.cl_super with
		| None -> error "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			print ctx "self = %s.new(%s" (ctx.type_accessor (TClassDecl c)) (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);
	| TField ({ eexpr = TConst TSuper },f) , params ->
		(match ctx.current.cl_super with
		| None -> error "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			let name = field_name f in
			(* TODO: use nonconflict var instead of mt *)
			print ctx "%s.mt%s.call(%s" (ctx.type_accessor (TClassDecl c)) (field name) (this ctx);
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
		spr ctx ".new";
		spr ctx "(";
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
		spr ctx "{";
		concat ctx "," (fun (name,data) ->
			spr ctx "{ ";
			spr ctx "name : ";
			gen_constant ctx e.epos (TString name);
			spr ctx ", data : ";
			gen_constant ctx e.epos (TString (Codegen.bytes_serialize data));
			spr ctx "}"
		) (Hashtbl.fold (fun name data acc -> (name,data) :: acc) ctx.com.resources []);
		spr ctx "}";
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
	| TField (e, ((FInstance _ | FAnon _) as ef)), el ->
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

and gen_expr ctx e =
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
		add_feature ctx "use.$iterator";
		print ctx "$iterator(";
		gen_value ctx x;
		print ctx ")";
	| TField (x,FClosure (Some ({cl_path=[],"Array"},_), {cf_name="push"})) ->
		(* see https://github.com/HaxeFoundation/haxe/issues/1997 *)
		add_feature ctx "use.$arrayPushClosure";
		print ctx "$arrayPushClosure(";
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
			print ctx "($_=";
			gen_value ctx x;
			print ctx ",_bind($_,$_%s))" (field f.cf_name))
	| TEnumParameter (x,_,i) ->
		gen_value ctx x;
		print ctx "{%i}" (i + 2)
	| TField ({ eexpr = TConst (TInt _ | TFloat _) } as x,f) ->
		gen_expr ctx { e with eexpr = TField(mk (TParenthesis x) x.etype x.epos,f) }
	| TField ( { eexpr = TConst(TInt _ | TFloat _| TString _| TBool _) } as e , ((FInstance _ | FAnon _) as ef)) ->
		spr ctx "(function(x) return x.";
		print ctx "%s" (field_name ef);
		spr ctx " end )(";
		gen_value ctx e;
		spr ctx ")";
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
		print ctx "goto _hx_continue_%i" ctx.continue_counter;
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
		gen_call ctx e el false
	| TArrayDecl el ->
		spr ctx "lua.Boot.defArray({";
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
		spr ctx ")";
	| TVar (v,eo) ->
		begin match eo with
			| None ->
				spr ctx "local ";
				spr ctx (ident v.v_name);
			| Some e ->
				match e.eexpr with
				| TBinop(OpAssign, e1, e2) ->
				    gen_tbinop ctx OpAssign e1 e2;
				    spr ctx "local ";
				    spr ctx (ident v.v_name);
				    spr ctx " = ";
				    gen_value ctx e1;

				| _ ->
				    spr ctx "local ";
				    spr ctx (ident v.v_name);
				    spr ctx " = ";
				    gen_value ctx e;
		end
	| TNew (c,_,el) ->
		print ctx "%s.new(" (ctx.type_accessor (TClassDecl c));
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if ";
		gen_value ctx cond;
		spr ctx " then ";
		gen_expr ctx e;
		newline ctx;
		(match eelse with
		| None -> print ctx "end";
		| Some e2 ->
			(match e.eexpr with
			| TObjectDecl _ -> ctx.separator <- false
			| _ ->());
			spr ctx "else ";
		newline ctx;
		let bend = open_block ctx in
		gen_expr ctx e2;
		bend();
		newline ctx;
		spr ctx "end");
	| TUnop ((Increment|Decrement) as op,unop_flag, e) ->
		spr ctx "(function() ";
		(match unop_flag with
		| Ast.Prefix ->
			gen_value ctx e;
			spr ctx " = ";
			gen_value ctx e;
			(match op with
			|Increment -> spr ctx " +"
			|Decrement -> spr ctx " -"
			|_-> print ctx " %s" (Ast.s_unop op));
			spr ctx " 1 return ";
			gen_value ctx e;
			spr ctx " end)()";
		| Ast.Postfix ->
			(* TODO: add guarantedd noconflict tmp variable instead of __x *)
			spr ctx "local __x = ";
			gen_value ctx e;
			newline ctx;
			gen_value ctx e;
			spr ctx " = ";
			gen_value ctx e;
			(match op with
			|Increment -> spr ctx " +"
			|Decrement -> spr ctx " -"
			|_-> print ctx " %s" (Ast.s_unop op));
			spr ctx " 1 return __x end)()");
	| TUnop (Not,unop_flag,e) ->
		spr ctx "not ";
		gen_value ctx e;
	| TUnop (NegBits,unop_flag,e) ->
		spr ctx "bit.bnot(";
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
		ctx.continue_counter <- ctx.continue_counter + 1;
		let id = ctx.continue_counter in
		spr ctx "while ";
		gen_value ctx cond;
		spr ctx " do ";
		gen_expr ctx e;
		handle_break();
		newline ctx;
		print ctx "::_hx_continue_%i::" id;
		newline ctx;
		spr ctx "end ";
		ctx.continue_counter <- ctx.continue_counter - 1;
	| TWhile (cond,e,Ast.DoWhile) ->
		let handle_break = handle_break ctx e in
		gen_expr ctx e;
		spr ctx "while ";
		gen_value ctx cond;
		spr ctx " do ";
		gen_expr ctx e;
		handle_break();
		spr ctx "end ";
	| TObjectDecl fields ->
		spr ctx "{ ";
		concat ctx ", " (fun (f,e) -> print ctx "%s = " (anon_field f); gen_value ctx e) fields;
		spr ctx "}";
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
		print ctx "while( %s.hasNext() ) do" it;
		let bend = open_block ctx in
		newline ctx;
		print ctx "local %s = %s.next()" (ident v.v_name) it;
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
				if !else_block then print ctx "{";
				if vname <> v.v_name then begin
					newline ctx;
					print ctx "local %s = %s" v.v_name vname;
				end;
				gen_block_element ctx e;
				if !else_block then begin
					newline ctx;
					print ctx "}";
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
				spr ctx " else ";
				else_block := true
		) catchs;
		if not !last then begin
		    print ctx "error(%s)" vname;
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
		| None -> ()
		| Some e ->
			spr ctx "else ";
			let bend = open_block ctx in
			gen_block_element ctx e;
			bend();
			newline ctx);
		spr ctx "end"
	| TCast (e,None) ->
		gen_expr ctx e
	| TCast (e1,Some t) ->
		print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
		gen_expr ctx e1;
		spr ctx " , ";
		spr ctx (ctx.type_accessor t);
		spr ctx ")"


and gen_block_element ?(after=false) ctx e =
	match e.eexpr with
	| TBinop (Ast.OpEq as op,e1,e2) ->
		spr ctx "(function() return ";
		gen_tbinop ctx op e1 e2;
		spr ctx " end)()";
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
	| TFunction _ ->
		gen_block_element ~after ctx (mk (TParenthesis e) e.etype e.epos)
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> gen_block_element ~after ctx e) fl
	| _ ->
		if not after then newline ctx;
		gen_expr ctx e;
		semicolon ctx;
		if after then newline ctx;

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
	| TCast (e1, None) ->
		gen_value ctx e1
	| TCast (e1, Some t) ->
		print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
		gen_value ctx e1;
		spr ctx " , ";
		spr ctx (ctx.type_accessor t);
		spr ctx ")"
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
				gen_expr ctx (assign e);
			| e :: l ->
				gen_expr ctx e;
				newline ctx;
				loop l
		in
		loop el;
		v();
	| TIf (cond,e,eo) ->
		let v = value() in
		spr ctx "if ";
		gen_value ctx cond;
		spr ctx " then ";
		gen_value ctx (assign e);
		let rec gen_elseif ctx e =
		(match e with
		| None->();
		| Some e2->
		    (match e2.eexpr with
		    | TIf(cond3, e3, eo3) ->
			spr ctx " elseif ";
			gen_value ctx cond3;
			spr ctx " then ";
			gen_expr ctx (assign e3);
			semicolon ctx;
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
		gen_expr ctx (mk (TTry (block (assign b),
			List.map (fun (v,e) -> v, block (assign e)) catchs
		)) e.etype e.epos);
		v()

and gen_tbinop ctx op e1 e2 =
    (match op with
    | Ast.OpAssign ->
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
    | Ast.OpAssignOp(op2) ->
        spr ctx "(function() "; gen_value ctx e1;
        spr ctx " = "; gen_tbinop ctx op2 e1 e2;
        spr ctx " return "; gen_value ctx e1;
        spr ctx " end)()";
    | Ast.OpXor | Ast.OpAnd  | Ast.OpShl | Ast.OpShr | Ast.OpUShr | Ast.OpOr ->
        gen_bitop ctx op e1 e2;
    | _->
        gen_value ctx e1;
        (match op with
            | Ast.OpAdd when (is_string_expr e1 || is_string_expr e2) ->
                    print ctx " .. "
            | Ast.OpNotEq -> print ctx " ~= "
            | Ast.OpBoolAnd -> print ctx " and "
            | Ast.OpBoolOr -> print ctx " or "
            | _ -> print ctx " %s " (Ast.s_binop op));
        gen_value ctx e2)

and gen_bitop ctx op e1 e2 =
    print ctx "bit.%s(" (match op with
        | Ast.OpXor  ->  "bxor"
        | Ast.OpAnd  ->  "band"
        | Ast.OpShl  ->  "lshift"
        | Ast.OpShr  ->  "rshift"
        | Ast.OpUShr ->  "arshift"
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
	    spr ctx "do return ";
	    (match e.eexpr with
		| TBinop(OpAssign, e1, e2) ->
			spr ctx "(function() ";
			gen_value ctx e;
			spr ctx " return ";
			gen_value ctx e1;
			spr ctx " end)()";
		| _ -> gen_value ctx e;
	    );
	    spr ctx " end")

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

let can_gen_class_field ctx = function
	| { cf_expr = (None | Some { eexpr = TConst TNull }) } when not (has_feature ctx "Type.getInstanceFields") ->
		false
	| f ->
		not (is_extern_field f)

let gen_class_field ctx c f =
	check_field_name c f;
	match f.cf_expr with
	| None ->
		print ctx "%s = nil" (anon_field f.cf_name);
		spr ctx ",";
		newline ctx;
	| Some e ->
		ctx.id_counter <- 0;
		(match e.eexpr with
		| TFunction f2 ->
		    let old = ctx.in_value, ctx.in_loop in
		    ctx.in_value <- None;
		    ctx.in_loop <- false;
		    print ctx "%s = function" (anon_field f.cf_name);
		    print ctx "(%s) " (String.concat "," ("self"::(List.map ident (List.map arg_name f2.tf_args))));
		    newline ctx;
		    let fblock = fun_block ctx f2 e.epos in
		    (match fblock.eexpr with
		    | TBlock el ->
			let rec loop ctx el = (match el with
			    | [hd] -> (match hd.eexpr with
				    | TReturn eo -> gen_return ctx e eo;
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
		    spr ctx "end,";
		    newline ctx;
		    ctx.in_value <- fst old;
		    ctx.in_loop <- snd old;
		    ctx.separator <- true;
		| _ -> gen_value ctx e);
		newline ctx

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
	ctx.continue_counter <- 0;
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
					spr ctx "self = {}";
					newline ctx;
					spr ctx "self.__methods = {}";
					newline ctx;
					spr ctx "setmetatable(self, {__index = lua.Boot.resolveMethod })";
					newline ctx;
					List.iter (gen_block_element ctx) el;
					newline ctx;
					(* TODO: use nonconflict var instead of mt *)
					print ctx "table.insert(self.__methods, %s.mt)" p;
					newline ctx;
					spr ctx "return self";
					bend();
					newline ctx;
				    |_ -> ());
				    spr ctx "end";
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

	let has_class = has_feature ctx "lua.Boot.getClass" && (c.cl_super <> None || c.cl_ordered_fields <> [] || c.cl_constructor <> None) in
	let has_prototype = c.cl_super <> None || has_class || List.exists (can_gen_class_field ctx) c.cl_ordered_fields in
	if has_prototype then begin
		(match c.cl_super with
		| None ->
			(* TODO: use nonconflict var instead of mt *)
			print ctx "%s.mt = {" p;
			newline ctx;
		| Some (csup,_) ->
			let psup = ctx.type_accessor (TClassDecl csup) in
			print ctx "%s.__super__ = %s" p psup;
			newline ctx;
			(* TODO: use nonconflict var instead of mt *)
			print ctx "%s.mt = {" p;
			newline ctx;
		);

		List.iter (fun f -> if can_gen_class_field ctx f then gen_class_field ctx c f) c.cl_ordered_fields;
		if has_class then begin
			newprop ctx;
			print ctx "__class__: %s" p;
		end;

		if has_property_reflection then begin
			let props = Codegen.get_properties c.cl_ordered_fields in
			(match c.cl_super with
			| _ when props = [] -> ()
			| Some (csup,_) when Codegen.has_properties csup ->
				newprop ctx;
				let psup = s_path ctx csup.cl_path in
				print ctx "__properties__ =  _extend(%s.prototype.__properties__,{%s})" psup (gen_props props)
			| _ ->
				newprop ctx;
				print ctx "__properties__ =  {%s}" (gen_props props));
		end;

		print ctx "\n}";
		newline ctx
	end

let generate_enum ctx e =
	let p = s_path ctx e.e_path in
	let ename = List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst e.e_path @ [snd e.e_path]) in
	print ctx "%s = " p;
	if has_feature ctx "Type.resolveEnum" then
	    spr ctx "_hxClasses[\"%s\"]";
	    newline ctx;
	    print ctx "_hxClasses[\"%s\"] = " (dot_path e.e_path);

	print ctx "{";
	if has_feature ctx "lua.Boot.isEnum" then print ctx " __ename__ = %s," (if has_feature ctx "Type.getEnumName" then "{" ^ String.concat "," ename ^ "}" else "true");
	print ctx " __constructs__ = {%s} }" (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" s) e.e_names));
	ctx.separator <- true;
	newline ctx;
	List.iter (fun n ->
		let f = PMap.find n e.e_constrs in
		print ctx "%s%s = " p (field f.ef_name);
		(match f.ef_type with
		| TFun (args,_) ->
			let sargs = String.concat "," (List.map (fun (n,_,_) -> ident n) args) in
			(* TODO: better tmp variable for _x, _estr *)
			print ctx "function(%s)  local _x = {\"%s\",%d,%s}; _x.__enum__ = %s;" sargs f.ef_name f.ef_index sargs p;
			if has_feature ctx "may_print_enum" then
				(* TODO: better namespacing for _estr *)
				spr ctx " _x.toString = _estr;";
			spr ctx " return _x; end ";
			ctx.separator <- true;
		| _ ->
			print ctx "{\"%s\",%d}" f.ef_name f.ef_index;
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
		print ctx "%s.__empty_constructs__ = {%s}" p (String.concat "," (List.map (fun s -> Printf.sprintf "%s.%s" p s) ctors_without_args));
		newline ctx
	end;
	match Codegen.build_metadata ctx.com (TEnumDecl e) with
	| None -> ()
	| Some e ->
		print ctx "%s.__meta__ = " p;
		gen_expr ctx e;
		newline ctx

let generate_static ctx (c,f,e) =
	print ctx "%s%s = " (s_path ctx c.cl_path) (static_field f);
	gen_value ctx e;
	newline ctx

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
		if not c.cl_extern then begin
		    generate_package_create ctx c.cl_path;
		    let p = s_path ctx c.cl_path in
		    print ctx "%s = {} " p;
		end
	| TEnumDecl e when e.e_extern ->
		()
	| TEnumDecl e ->
		generate_package_create ctx e.e_path;
		let p = s_path ctx e.e_path in
		print ctx "%s = {} " p;
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
		in_loop = false;
		handle_break = false;
		id_counter = 0;
		continue_counter = 0;
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

	spr ctx "pcall(require, 'bit32')"; newline ctx;
	spr ctx "pcall(require, 'bit')"; newline ctx; newline ctx;
	spr ctx "bit = bit or bit32"; newline ctx;
	spr ctx "bit32 = bit"; newline ctx; newline ctx;


	let vars = [] in
	let vars = (if has_feature ctx "Type.resolveClass" || has_feature ctx "Type.resolveEnum" then ("_hxClasses = " ^ "{}") :: vars else vars) in
	let vars = if has_feature ctx "may_print_enum"
		then ("_estr = function()  return " ^ (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" })) ^ ".__string_rec(self,''); end") :: vars
		else vars in
	(match List.rev vars with
	| [] -> ()
	| vl ->
		print ctx "local %s" (String.concat ";" vl);
		ctx.separator <- true;
		newline ctx
	);

	List.iter (generate_type_forward ctx) com.types;
	newline ctx;

	List.iter (generate_type ctx) com.types;
	let rec chk_features e =
		if is_dynamic_iterator ctx e then add_feature ctx "use.$iterator";
		match e.eexpr with
		| TField (_,FClosure _) ->
			add_feature ctx "use._bind"
		| _ ->
			Type.iter chk_features e
	in
	List.iter chk_features ctx.inits;
	List.iter (fun (_,_,e) -> chk_features e) ctx.statics;
	if has_feature ctx "use.$iterator" then begin
		add_feature ctx "use._bind";
		print ctx "function $iterator(o) { if( o instanceof Array ) return function() { return HxOverrides.iter(o); }; return typeof(o.iterator) == 'function' ? _bind(o,o.iterator) : o.iterator; }";
		newline ctx;
	end;
	if has_feature ctx "use._bind" then begin
		print ctx "var $_, $fid = 0";
		newline ctx;
		print ctx "function _bind(o,m) { if( m == nil ) return nil; if( m.__id__ == nil ) m.__id__ = $fid++; var f; if( o.hx__closures__ == nil ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == nil ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; }";
		newline ctx;
	end;
	if has_feature ctx "use.$arrayPushClosure" then begin
		print ctx "function $arrayPushClosure(a) {";
		print ctx " return function(x) { a.push(x); }; ";
		print ctx "}";
		newline ctx
	end;
	List.iter (gen_block_element ~after:true ctx) (List.rev ctx.inits);
	List.iter (generate_static ctx) (List.rev ctx.statics);
	(match com.main with
	| None -> ()
	| Some e -> gen_expr ctx e; newline ctx);
	let ch = open_out_bin com.file in
	output_string ch (Buffer.contents ctx.buf);
	close_out ch;
	t()

