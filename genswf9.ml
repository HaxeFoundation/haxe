(*
 *  Haxe Compiler
 *  Copyright (c)2006 Nicolas Cannasse
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
open As3
open Type

type ('a,'b) gen_lookup = {
	h : ('a,'b) Hashtbl.t;
	a : 'a DynArray.t;
	c : int -> 'b;
}

type 'a lookup = ('a,'a index) gen_lookup
type 'a lookup_nz = ('a,'a index_nz) gen_lookup

type context = {
	(* globals *)
	idents : string lookup;
	ints : int32 lookup;
	floats : float lookup;
	brights : as3_base_right lookup;
	rights : as3_rights lookup;
	types : as3_type lookup;
	mtypes : as3_method_type lookup_nz;
	mutable classes : as3_class list;
	mutable statics : as3_static list;
	mutable inits : as3_static list;
	functions : as3_function lookup;
	rpublic : as3_base_right index;

	(* per-function *)
	mutable locals : (string,int) PMap.t;
	mutable code : as3_opcode DynArray.t;
	mutable pos : int;
}

let public = A3RPublic None
let mt0 = {
	mt3_ret = None;
	mt3_args = [];
	mt3_native = false;
	mt3_var_args = false;
	mt3_debug_name = None;
	mt3_dparams = None;
	mt3_pnames = None;
	mt3_unk_flags = (false,false,false,false);
}

let index_int (x : int) : 'a index = Obj.magic (x + 1)
let index_nz_int (x : int) : 'a index_nz = Obj.magic x
let tid (x : 'a index) : int = Obj.magic x

let new_lookup() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_int }
let new_lookup_nz() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_nz_int }

let lookup i w =
	try
		Hashtbl.find w.h i
	with
		Not_found ->
			let id = w.c (DynArray.length w.a) in
			Hashtbl.add w.h i id;
			DynArray.add w.a i;
			id

let add i w =
	let id = w.c (DynArray.length w.a) in
	DynArray.add w.a i;
	id

let lookup_array w = DynArray.to_array w.a

let ident ctx i = lookup i ctx.idents

let write ctx op =
	DynArray.add ctx.code op;
	ctx.pos <- As3code.length op + ctx.pos

let acc_ident ctx i =
	try
		write ctx (A3Reg (PMap.find i ctx.locals))
	with
		Not_found -> assert false

let type_path ctx ?(getclass=false) (pack,name) =
	let pid = ident ctx (String.concat "." pack) in
	let nameid = ident ctx name in
	let pid = lookup (A3RPublic (Some pid)) ctx.brights in
	let tid = lookup (if getclass then A3TClassInterface (Some nameid,pid) else A3TMethodVar (nameid,pid)) ctx.types in
	tid

let begin_fun ctx args =
	let mt = {
		mt3_ret = None;
		mt3_args = List.map (fun _ -> None) args;
		mt3_native = false;
		mt3_var_args = false;
		mt3_debug_name = None;
		mt3_dparams = None;
		mt3_pnames = None;
		mt3_unk_flags = (false,false,false,false);
	} in
	let old_locals = ctx.locals in
	let old_code = ctx.code in
	let old_pos = ctx.pos in
	let count = ref 0 in
	ctx.locals <- List.fold_left (fun acc name -> incr count; PMap.add name (!count) acc) PMap.empty args;
	ctx.code <- DynArray.create();
	ctx.pos <- 0;
	(fun () ->
		let f = {
			fun3_id = add mt ctx.mtypes;
			fun3_unk1 = 2;
			fun3_unk2 = 1;
			fun3_unk3 = 1;
			fun3_unk4 = 3;
			fun3_code = DynArray.to_list ctx.code;
			fun3_trys = [||];
			fun3_locals = [||];
		} in
		ignore(add f ctx.functions);
		ctx.locals <- old_locals;
		ctx.code <- old_code;
		ctx.pos <- old_pos;
		f.fun3_id
	)

let generate_construct ctx args =
	let f = begin_fun ctx args in
	write ctx A3This;
	write ctx A3Context;
	write ctx A3This;
	List.iter (acc_ident ctx) args;
	write ctx (A3SuperConstr (List.length args));
	write ctx A3RetVoid;
	f()

let generate_class_init ctx c =
	let f = begin_fun ctx [] in
	write ctx A3This;
	write ctx A3Context;
	write ctx (A3LoadBlock 0);
	write ctx (A3GetProp (tid (type_path ctx ([],"Object"))));
	write ctx A3Context;
	write ctx (A3GetProp (tid (type_path ~getclass:true ctx ([],"Object"))));
	write ctx (A3ClassDef (List.length ctx.classes));
	write ctx A3PopContext;
	write ctx (A3Set (tid (type_path ctx c.cl_path)));
	write ctx A3RetVoid;
	f()

let generate_class_static ctx c =
	let f = begin_fun ctx [] in
	write ctx A3RetVoid;
	f()

let generate_class ctx c =
	let name_id = type_path ctx c.cl_path in
	let st_id = generate_class_static ctx c in
	let cid = (match c.cl_constructor with
		| None ->
			let rec loop c =
				match c.cl_super with
				| None ->
					generate_construct ctx []
				| Some (csup,_) ->
					match csup.cl_constructor with
					| None -> loop csup
					| Some co -> generate_construct ctx (match follow co.cf_type with TFun (l,_) -> List.map (fun (name,_,_) -> name) l | _ -> assert false)
			in
			loop c
		| Some f -> assert false
	) in
	let fields = [||] in
	let sc = {
		cl3_name = name_id;
		cl3_super = (match c.cl_super with None -> Some (type_path ctx ([],"Object")) | Some _ -> assert false);
		cl3_sealed = true;
		cl3_final = false;
		cl3_interface = false;
		cl3_rights = None;
		cl3_implements = [||];
		cl3_construct = cid;
		cl3_fields = fields;
	} in
	let st = {
		st3_method = st_id;
		st3_fields = [||];
	} in
	let ic = {
		st3_method = generate_class_init ctx c;
		st3_fields = [|
			{
				f3_name = sc.cl3_name;
				f3_slot = 1;
				f3_kind = A3FClass (index_nz_int (List.length ctx.classes));
				f3_metas = None;
			}
		|];
	} in	
	ctx.classes <- sc :: ctx.classes;
	ctx.statics <- st :: ctx.statics;
	ctx.inits <- ic :: ctx.inits;
	()

let generate_type ctx t =
	match t with
	| TClassDecl c -> if not c.cl_extern then generate_class ctx c
	| TTypeDecl _ -> ()
	| TEnumDecl e ->
		match e.e_path with
		| [] , "Void" | [] , "Bool" | [] , "Dynamic" -> ()
		| _ ->
			failwith (Ast.s_type_path e.e_path)

let generate types hres =
	let brights = new_lookup() in
	let idents = new_lookup() in
	let empty_id = lookup "" idents in
	let rpublic = lookup (A3RPublic (Some empty_id)) brights in
	let ctx = {
		idents = idents;
		ints = new_lookup();
		floats = new_lookup();
		brights = brights;
		rights = new_lookup();
		types = new_lookup();
		mtypes = new_lookup_nz();
		rpublic = rpublic;
		classes = [];
		statics = [];
		inits = [];
		functions = new_lookup();

		code = DynArray.create();
		locals = PMap.empty;
		pos = 0;
	} in
	ignore(lookup [ctx.rpublic] ctx.rights);
	List.iter (generate_type ctx) types;
	Hashtbl.iter (fun _ _ -> assert false) hres;
	let a = {
		as3_ints = lookup_array ctx.ints;
		as3_floats = lookup_array ctx.floats;
		as3_idents = lookup_array ctx.idents;
		as3_base_rights = lookup_array ctx.brights;
		as3_rights = lookup_array ctx.rights;
		as3_types = lookup_array ctx.types;
		as3_method_types = lookup_array ctx.mtypes;
		as3_metadatas = [||];
		as3_classes = Array.of_list (List.rev ctx.classes);
		as3_statics = Array.of_list (List.rev ctx.statics);
		as3_inits = Array.of_list (List.rev ctx.inits);
		as3_functions = lookup_array ctx.functions;
		as3_unknown = "";
	} in
	[Swf.TActionScript3 (None,a); Swf.TSwf9Name [0,"Test"]]
