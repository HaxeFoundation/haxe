(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
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
open Type

type context = {
	(* shared *)	
	types : (module_path, module_path) Hashtbl.t;
	modules : (module_path , module_def) Hashtbl.t;
	delays : (unit -> unit) list list ref;
	(* per-module *)
	current : module_def;
	mutable in_static : bool;
	mutable local_types : (module_path * module_type) list;
}

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

let context() = {
	modules = Hashtbl.create 0;
	types = Hashtbl.create 0;
	delays = ref [];
	in_static = false;
	local_types = [];
	current = {
		mpath = [] , "";
		mtypes = [];
	}
}

type error_msg =
	| Module_not_found of module_path
	| Cannot_unify of t * t
	| Custom of string

exception Error of error_msg * pos

let error_msg = function
	| Module_not_found m -> "Module not found : " ^ s_type_path m
	| Cannot_unify (t1,t2) -> 
		let ctx = print_context() in
		s_type ctx t1 ^ " should be " ^ s_type ctx t2
	| Custom s -> s

let error msg p = raise (Error (msg,p))

let load_ref : (context -> module_path -> pos -> module_def) ref = ref (fun _ _ _ -> assert false)

let load ctx m p = (!load_ref) ctx m p

(** since load_type is used in PASS2 , it cannot access the structure of a type **)

let load_type_def ctx tpath p =
	try
		snd (List.find (fun (tp,_) -> tp = tpath || (fst tpath = [] && snd tp = snd tpath)) ctx.local_types)
	with
		Not_found ->
			let m = load ctx tpath p in
			try
				snd (List.find (fun (tp,_) -> tp = tpath) m.mtypes)
			with
				Not_found -> error (Custom ("Module " ^ s_type_path tpath ^ " does not define type " ^ snd tpath)) p

let rec load_normal_type ctx t p =
	match load_type_def ctx (t.tpackage,t.tname) p with
	| TClassDecl c -> 
		if List.length c.cl_types <> List.length t.tparams then error (Custom ("Invalid number of type parameters for " ^ s_type_path (fst c.cl_module,c.cl_name))) p;
		let types = List.map (fun t ->
			let t = load_type ctx t p in
			(** CHECK t AGAINST corresponding classtype (for subtyping) **)
			t
		) t.tparams in
		TInst (c,types)
	| TEnumDecl ->
		assert false

and load_type ctx t p =
	match t with
	| TPNormal t -> load_normal_type ctx t p
	| TPAnonymous l -> assert false
	| TPFunction (args,r) -> assert false

let load_type_opt ctx t p =
	match t with
	| None -> TMono (ref None)
	| Some t -> load_type ctx t p

let unify t1 t2 p =
	if not (unify t1 t2) then error (Cannot_unify (t1,t2)) p

let t_int ctx = load_normal_type ctx { tpackage = []; tname = "Int"; tparams = [] } null_pos
let t_float ctx = load_normal_type ctx { tpackage = []; tname = "Float"; tparams = [] } null_pos
let t_bool ctx = load_normal_type ctx { tpackage = []; tname = "Bool"; tparams = [] } null_pos
let t_string ctx = load_normal_type ctx { tpackage = []; tname = "String"; tparams = [] } null_pos

let t_array ctx =
	match load_type_def ctx ([],"Array") null_pos with
	| TClassDecl c ->
		if List.length c.cl_types <> 1 then assert false;
		let pt = TMono (ref None) in
		TInst (c,[pt]) , pt
	| _ ->
		assert false

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let type_ident ctx i p =
	assert false

let type_constant ctx c p =
	match c with
	| Int i -> mk (TConst (TInt i)) (t_int ctx) p
	| Float f -> mk (TConst (TFloat f)) (t_float ctx) p 
	| String s -> mk (TConst (TString s)) (t_string ctx) p
	| Ident s -> type_ident ctx s p
	| Type s ->
		let t = load_type_def ctx ([],s) p in 
		assert false

let rec type_binop ctx op e1 e2 p =
	assert false

and type_expr ctx (e,p) =
	match e with
	| EConst c ->
		type_constant ctx c p
	| EArray (e1,e2) ->
		let e1 = type_expr ctx e1 in
		let e2 = type_expr ctx e2 in
		unify e2.etype (t_int ctx) e2.epos;
		let t , pt = t_array ctx in
		unify e1.etype t e1.epos;
		mk (TArray (e1,e2)) pt p
    | EBinop (op,e1,e2) -> 
		type_binop ctx op e1 e2 p
	| _ ->
		assert false
(*/*
	| EField of expr * string
	| EType of expr * string
	| EParenthesis of expr
	| EObjectDecl of (string * expr) list
	| EArrayDecl of expr list
	| ECall of expr * expr list
	| ENew of type_path * expr list
	| EUnop of unop * unop_flag * expr
	| EVars of (string * type_path option * expr option) list
	| EFunction of func
	| EBlock of expr list
	| EFor of string * expr * expr
	| EIf of expr * expr * expr option
	| EWhile of expr * expr * while_flag
	| ESwitch of expr * (expr * expr) list * expr option
	| ETry of expr * (string * type_path * expr) list
	| EReturn of expr option
	| EBreak
	| EContinue
*/*)

let type_static_var ctx t e p =
	ctx.in_static <- true;
	let e = type_expr ctx e in
	unify e.etype t p;
	e

let type_function ctx t static f p =
	ctx.in_static <- static;
	let e = type_expr ctx f.f_expr in
	unify e.etype t p;
	e

let check_overloading c p () =
	let rec loop s f =
		match s with
		| None -> ()
		| Some (c,_) ->
			try
				let f2 = PMap.find f.cf_name c.cl_fields in
				if not (type_eq f.cf_type f2.cf_type) then error (Custom ("Field " ^ f.cf_name ^ " overload parent class with different or incomplete type")) p;
				if f.cf_public <> f2.cf_public then error (Custom ("Field " ^ f.cf_name ^ " have different access right than previous one")) p; 
			with
				Not_found -> loop c.cl_super f
	in
	PMap.iter (fun _ f -> loop c.cl_super f) c.cl_fields

(* ---------------------------------------------------------------------- *)
(* PASS 1 & 2 : Module and Class Structure *)

let init_class ctx c p types herits fields =
	let type_class_flags n flags =
		match flags with
		| [] -> TEnum ((fst ctx.current.mpath,c.cl_name ^ "#" ^ n),[])
		| l -> assert false
	in
	c.cl_types <- List.map (fun (n,flags) ->
		n , type_class_flags n flags
	) types;
	c.cl_native <- List.mem HNative herits;
	let rec loop_super = function 
		| [] ->
			None
		| HExtends t :: _ ->
			let t = load_normal_type ctx t p in
			(match t with
			| TInst (cl,params) -> Some (cl,params)
			| _ -> error (Custom "Should extend a class") p)
		| _ :: l ->
				loop_super l
	in
	c.cl_super <- loop_super herits;
	let rec loop_implements = function
		| [] -> []
		| HImplements t :: l ->
			let t = load_normal_type ctx t p in
			(match t with
			| TInst (cl,params) -> (cl, params) :: loop_implements l
			| _ -> error (Custom "Shoule implement a class") p)
		| _ :: l ->
			loop_implements l
	in
	c.cl_implements <- loop_implements herits;
	let loop_cf f p =
		match f with
		| FVar (name,access,t,e) ->
			let t = load_type ctx t p in
			let cf = {
				cf_name = name;
				cf_type = t;
				cf_expr = None;
				cf_public = not (List.mem APrivate access);
			} in
			let delay = (match e with 
				| None -> (fun() -> ())
				| Some e -> (fun () -> cf.cf_expr <- Some (type_static_var ctx t e p))
			) in
			List.mem AStatic access, cf, delay
		| FFun (name,access,f) ->
			let t = TFun (List.map (fun (_,t) -> load_type_opt ctx t p) f.f_args,load_type_opt ctx f.f_type p) in
			let stat = List.mem AStatic access in
			let cf = {
				cf_name = name;
				cf_type = t;
				cf_expr = None;
				cf_public = not (List.mem APrivate access);
			} in
			stat, cf , (fun() -> cf.cf_expr <- Some (type_function ctx t stat f p))
	in
	List.map (fun (f,p) ->
		let static , f , delayed = loop_cf f p in
		if PMap.mem f.cf_name (if static then c.cl_statics else c.cl_fields) then error (Custom ("Duplicate class field declaration : " ^ f.cf_name)) p;
		if static then
			c.cl_statics <- PMap.add f.cf_name f c.cl_statics
		else
			c.cl_fields <- PMap.add f.cf_name f c.cl_fields;
		delayed
	) fields

let type_module ctx m tdecls =
	(* PASS 1 : build module structure - does not load any module or type - should be atomic ! *)
	let decls = ref [] in
	let decl_with_name name p =
		let tpath = (fst m,name) in
		try
			let m2 = Hashtbl.find ctx.types tpath in
			error (Custom ("Type name " ^ s_type_path tpath ^ " is redefined from module " ^ s_type_path m2)) p
		with
			Not_found ->
				Hashtbl.add ctx.types (fst m,name) m
	in
	List.iter (fun (d,p) ->
		match d with
		| EImport _ -> ()
		| EClass (name,_,_,_) ->
			decl_with_name name p;
			let c = { 
				cl_module = m;
				cl_name = name;
				cl_types = [];
				cl_native = false;
				cl_super = None;
				cl_implements = [];
				cl_fields = PMap.empty;
				cl_statics = PMap.empty;
			} in
			decls := ((fst m,name),TClassDecl c) :: !decls
		| EEnum (name,_,_) ->
			decl_with_name name p;
			assert false
	) tdecls;
	let m = {
		mpath = m;
		mtypes = List.rev !decls;
	} in
	Hashtbl.add ctx.modules m.mpath m;
	(* PASS 2 : build types structure - does not type any expression ! *)
	let ctx = {
		modules = ctx.modules;
		delays = ctx.delays;
		types = ctx.types;

		current = m;
		local_types = m.mtypes;
		in_static = false;
	} in
	let delays = ref [] in
	List.iter (fun (d,p) ->
		match d with
		| EImport t ->
			let m = load ctx t p in
			ctx.local_types <- ctx.local_types @ m.mtypes
		| EClass (name,types,herits,fields) ->
			let c = List.find (fun (_,d) -> match d with TClassDecl ({ cl_name = n } as c) -> n = name | _ -> false) m.mtypes in
			let c = (match snd c with TClassDecl c -> c | _ -> assert false) in
			delays := !delays @ check_overloading c p :: init_class ctx c p types herits fields
		| EEnum (name,types,constrs) ->
			assert false
	) tdecls;
	(* PASS 3 : type checking, delayed until all modules and types are built *)
	ctx.delays := !delays :: !(ctx.delays);
	m

let load ctx m p =
	try
		Hashtbl.find ctx.modules m
	with
		Not_found ->
			let file = (match m with [] , name -> name | l , name -> String.concat "/" l ^ "/" ^ name) ^ ".hx" in
			let file = (try Plugin.find_file file with Not_found -> error (Module_not_found m) p) in
			let ch = (try open_in file with _ -> error (Custom ("Could not open " ^ file)) p) in
			let pack , decls = (try Parser.parse (Lexing.from_channel ch) file with e -> close_in ch; raise e) in
			close_in ch;
			if pack <> fst m then begin
				let spack m = if m = [] then "<empty>" else String.concat "." m in
				if p == Ast.null_pos then
					error (Custom ("Invalid commandline class : " ^ s_type_path m ^ " should be " ^ s_type_path (pack,snd m))) p
				else
					error (Custom ("Invalid package : " ^ spack (fst m) ^ " should be " ^ spack pack)) p
			end;
			type_module ctx m decls

let rec finalize ctx =
	let delays = List.concat !(ctx.delays) in
	ctx.delays := [];
	match delays with
	| [] -> () (* at last done *)
	| l ->
		List.iter (fun f -> f()) l;
		finalize ctx
;;
load_ref := load
