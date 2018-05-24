(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*)
open Ast
open Globals
open Error
open Typecore
open Type
open CompletionItem
open ClassFieldOrigin

let get_submodule_fields ctx path =
	let m = Hashtbl.find ctx.g.modules path in
	let tl = List.filter (fun t -> path <> (t_infos t).mt_path && not (t_infos t).mt_private) m.m_types in
	let tl = List.map (fun mt ->
		ITType(CompletionItem.CompletionModuleType.of_module_type mt,ImportStatus.Imported)
	) tl in
	tl

let collect_static_extensions ctx items e p =
	let opt_type t =
		match t with
		| TLazy f ->
			return_partial_type := true;
			let t = lazy_type f in
			return_partial_type := false;
			t
		| _ ->
			t
	in
	let rec loop acc = function
		| [] ->
			acc
		| (c,_) :: l ->
			let rec dup t = Type.map dup t in
			let acc = List.fold_left (fun acc f ->
				if Meta.has Meta.NoUsing f.cf_meta || Meta.has Meta.Impl f.cf_meta || PMap.mem f.cf_name items then
					acc
				else begin
					let f = { f with cf_type = opt_type f.cf_type } in
					let monos = List.map (fun _ -> mk_mono()) f.cf_params in
					let map = apply_params f.cf_params monos in
					match follow (map f.cf_type) with
					| TFun((_,_,TType({t_path=["haxe";"macro"], "ExprOf"}, [t])) :: args, ret)
					| TFun((_,_,t) :: args, ret) ->
						begin try
							unify_raise ctx (dup e.etype) t e.epos;
							List.iter2 (fun m (name,t) -> match follow t with
								| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
									List.iter (fun tc -> unify_raise ctx m (map tc) e.epos) constr
								| _ -> ()
							) monos f.cf_params;
							if not (can_access ctx c f true) || follow e.etype == t_dynamic && follow t != t_dynamic then
								acc
							else begin
								let f = prepare_using_field f in
								let f = { f with cf_params = []; cf_public = true; cf_type = TFun(args,ret) } in
								let origin = StaticExtension(TClassDecl c) in
								let item = ITClassField (CompletionClassField.make f CFSMember origin true) in
								PMap.add f.cf_name item acc
							end
						with Error (Unify _,_) ->
							acc
						end
					| _ ->
						acc
				end
			) acc c.cl_ordered_statics in
			loop acc l
	in
	match follow e.etype with
	| TMono _ ->
		items
	| _ ->
		let items = loop items ctx.m.module_using in
		let items = loop items ctx.g.global_using in
		items

let collect ctx e_ast e dk with_type p =
	let opt_args args ret = TFun(List.map(fun (n,o,t) -> n,true,t) args,ret) in
	let should_access c cf stat =
		if Meta.has Meta.NoCompletion cf.cf_meta then false
		else if c != ctx.curclass && not cf.cf_public && String.length cf.cf_name > 4 then begin match String.sub cf.cf_name 0 4 with
			| "get_" | "set_" -> false
			| _ -> can_access ctx c cf stat
		end else
			(not stat || not (Meta.has Meta.Impl cf.cf_meta)) &&
			can_access ctx c cf stat
	in
	let rec loop items t =
		let is_new_item items name = not (PMap.mem name items) in
		match follow t with
		| TInst ({cl_kind = KTypeParameter tl},_) ->
			(* Type parameters can access the fields of their constraints *)
			List.fold_left (fun acc t -> loop acc t) items tl
		| TInst(c0,tl) ->
			(* For classes, browse the hierarchy *)
			let fields = TClass.get_all_fields c0 tl in
			PMap.foldi (fun k (c,cf) acc ->
				if should_access c cf false && is_new_item acc cf.cf_name then begin
					let origin = if c == c0 then Self(TClassDecl c) else Parent(TClassDecl c) in
				 	let item = ITClassField (CompletionClassField.make cf CFSMember origin true) in
					PMap.add k item acc
				end else
					acc
			) fields items
		| TAbstract({a_impl = Some c} as a,tl) ->
			(* Abstracts should show all their @:impl fields minus the constructor. *)
			let items = List.fold_left (fun acc cf ->
				if Meta.has Meta.Impl cf.cf_meta && should_access c cf false && is_new_item acc cf.cf_name then begin
					let origin = Self(TAbstractDecl a) in
					let cf = prepare_using_field cf in
					let cf = if tl = [] then cf else {cf with cf_type = apply_params a.a_params tl cf.cf_type} in
					let item = ITClassField (CompletionClassField.make cf CFSMember origin true) in
					PMap.add cf.cf_name item acc
				end else
					acc
			) items c.cl_ordered_statics in
			begin try
				(* If there's a @:forward, get the fields of the underlying type and filter them. *)
				let _,el,_ = Meta.get Meta.Forward a.a_meta in
				let sl = ExtList.List.filter_map (fun e -> match fst e with
					| EConst(Ident s) -> Some s
					| _ -> None
				) el in
				let forwarded_fields = loop PMap.empty (apply_params a.a_params tl a.a_this) in
				if sl = [] then items else PMap.foldi (fun name item acc ->
					if List.mem name sl && is_new_item acc name then
						PMap.add name item acc
					else
						acc
				) forwarded_fields items
			with Not_found ->
				items
			end
		| TAnon an ->
			(* Anons only have their own fields. *)
			PMap.foldi (fun name cf acc ->
				if is_new_item acc name then begin
					let origin,check = match !(an.a_status) with
						| Statics c -> Self (TClassDecl c),should_access c cf true
						| EnumStatics en -> Self (TEnumDecl en),true
						| AbstractStatics a ->
							let check = match a.a_impl with
								| None -> true
								| Some c -> should_access c cf true
							in
							Self (TAbstractDecl a),check
						| _ -> AnonymousStructure an,true
					in
					if check then PMap.add name (ITClassField (CompletionClassField.make cf CFSMember origin true)) acc
					else acc
				end else
					acc
			) an.a_fields items
		| TFun (args,ret) ->
			(* A function has no field except the magic .bind one. *)
			if is_new_item items "bind" then begin
				let t = opt_args args ret in
				let cf = mk_field "bind" (tfun [t] t) p null_pos in
				cf.cf_kind <- Method MethNormal;
				let item = ITClassField (CompletionClassField.make cf CFSStatic BuiltIn true) in
				PMap.add "bind" item items
			end else
				items
		| _ ->
			items
	in
	(* Add special `.code` field if we have a string of length 1 *)
	let items = match fst e_ast with
		| EConst(String s) when String.length s = 1 ->
			let cf = mk_field "code" ctx.t.tint e.epos null_pos in
			cf.cf_doc <- Some "The character code of this character (inlined at compile-time).";
			cf.cf_kind <- Var { v_read = AccNormal; v_write = AccNever };
			let item = ITClassField (CompletionClassField.make cf CFSStatic BuiltIn true) in
			PMap.add cf.cf_name item PMap.empty
		| _ ->
			PMap.empty
	in
	(* Collect fields of the type *)
	let items = loop items e.etype in
	(* Add static extensions *)
	let items = collect_static_extensions ctx items e p in
	let items = PMap.fold (fun item acc -> item :: acc) items [] in
	try
		let sl = string_list_of_expr_path_raise e_ast in
		(* Add submodule fields *)
		items @ get_submodule_fields ctx (List.tl sl,List.hd sl)
	with Exit | Not_found ->
		items
