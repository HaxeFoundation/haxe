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
	modules : (module_path , module_def) Hashtbl.t;
	current : module_def;
	delays : (unit -> unit) list list ref;
	mutable types : (module_path * module_type) list;
}

let context() = {
	modules = Hashtbl.create 0;
	types = [];
	delays = ref [];
	current = {
		mpath = [] , "";
		mtypes = [];
	}
}

type error_msg =
	| Module_not_found of module_path
	| Custom of string

exception Error of error_msg * pos

let error_msg = function
	| Module_not_found m -> "Module not found : " ^ s_type_path m
	| Custom s -> s

let error msg p = raise (Error (msg,p))

let load_ref : (context -> module_path -> pos -> module_def) ref = ref (fun _ _ _ -> assert false)

let load ctx m p = (!load_ref) ctx m p

(** since load_type is used in PASS2 , it cannot access the structure of a type **)
let rec load_normal_type ctx t p =
	let tpath = t.tpackage , t.tname in
	let tdef = (try
		snd (List.find (fun (tp,_) -> tp = tpath || snd tp = t.tname) ctx.types)
	with
		Not_found ->
			let m = load ctx tpath p in
			try
				snd (List.find (fun (tp,_) -> tp = tpath) m.mtypes)
			with
				Not_found -> error (Custom ("Module " ^ s_type_path tpath ^ " does not define type " ^ t.tname)) p
	) in
	match tdef with
	| TClassDecl c -> 
		if List.length c.cl_types <> List.length t.tparams then error (Custom ("Invalid number of type parameters for " ^ s_type_path tpath)) p;
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

let type_static_var ctx t e p () =
	assert false

let type_function ctx t static f p () =
	assert false

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

let init_class ctx c p types herits fields =
	let type_class_flags n flags =
		match flags with
		| [] -> TEnum (fst ctx.current.mpath,c.cl_name ^ "#" ^ n)
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
			List.mem AStatic access, {
				cf_name = name;
				cf_type = t;
				cf_expr = None;
				cf_public = not (List.mem APrivate access);
			} , (match e with None -> (fun() -> ()) | Some e -> type_static_var ctx t e p)
		| FFun (name,access,f) ->
			let t = TFun (List.map (fun (_,t) -> load_type_opt ctx t p) f.f_args,load_type_opt ctx f.f_type p) in
			let stat = List.mem AStatic access in
			stat, {
				cf_name = name;
				cf_type = t;
				cf_expr = None;
				cf_public = not (List.mem APrivate access);
			} , type_function ctx t stat f p
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
		List.iter (fun ((_,name2),_) ->
			if name = name2 then error (Custom ("Type name redefinition " ^ name)) p;
		) (!decls)
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
		current = m;
		types = m.mtypes;
	} in
	let delays = ref [] in
	List.iter (fun (d,p) ->
		match d with
		| EImport t ->
			let m = load ctx t p in
			ctx.types <- ctx.types @ m.mtypes
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