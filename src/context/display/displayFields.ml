(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

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
open Common
open Typecore
open Type
open CompletionItem
open ClassFieldOrigin
open DisplayTypes
open Display

let get_submodule_fields ctx path =
	let m = Hashtbl.find ctx.g.modules path in
	let tl = List.filter (fun t -> path <> (t_infos t).mt_path && not (t_infos t).mt_private) m.m_types in
	let tl = List.map (fun mt ->
		make_ci_type (CompletionItem.CompletionModuleType.of_module_type mt) ImportStatus.Imported None
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
	let rec dup t = Type.map dup t in
	let handle_field c f acc =
		let f = { f with cf_type = opt_type f.cf_type } in
		let monos = List.map (fun _ -> spawn_monomorph ctx p) f.cf_params in
		let map = apply_params f.cf_params monos in
		match follow (map f.cf_type) with
		| TFun((_,_,TType({t_path=["haxe";"macro"], "ExprOf"}, [t])) :: args, ret)
		| TFun((_,_,t) :: args, ret) ->
			begin try
				let e = TyperBase.unify_static_extension ctx {e with etype = dup e.etype} t p in
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> unify_raise ctx m (map tc) e.epos) constr
					| _ -> ()
				) monos f.cf_params;
				if not (can_access ctx c f true) || follow e.etype == t_dynamic && follow t != t_dynamic then
					acc
				else begin
					let f = prepare_using_field f in
					let f = { f with cf_params = []; cf_flags = set_flag f.cf_flags (int_of_class_field_flag CfPublic); cf_type = TFun(args,ret) } in
					let decl = match c.cl_kind with
						| KAbstractImpl a -> TAbstractDecl a
						| _ -> TClassDecl c
					in
					let origin = StaticExtension(decl) in
					let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta f.cf_meta) f.cf_type in
					let item = make_ci_class_field (CompletionClassField.make f CFSMember origin true) (f.cf_type,ct) in
					PMap.add f.cf_name item acc
				end
			with Error (Unify _,_) | Unify_error _ ->
				acc
			end
		| _ ->
			acc
		in
	let rec loop acc = function
		| [] ->
			acc
		| (c,_) :: l ->
			let acc = List.fold_left (fun acc f ->
				if Meta.has Meta.NoUsing f.cf_meta || Meta.has Meta.NoCompletion f.cf_meta || has_class_field_flag f CfImpl || PMap.mem f.cf_name acc then
					acc
				else
					List.fold_left (fun acc f -> handle_field c f acc) acc (f :: f.cf_overloads)
			) acc c.cl_ordered_statics in
			loop acc l
	in
	match follow e.etype with
	| TMono _ ->
		items
	| _ ->
		let items = loop items ctx.m.module_using in
		let items = loop items ctx.g.global_using in
		let items = try
			let mt = module_type_of_type e.etype in
			loop items (t_infos mt).mt_using
		with Exit ->
			items
		in
		items

let collect ctx e_ast e dk with_type p =
	let opt_args args ret = TFun(List.map(fun (n,o,t) -> n,true,t) args,ret) in
	let should_access c cf stat =
		if Meta.has Meta.NoCompletion cf.cf_meta then false
		else if c != ctx.curclass && not (has_class_field_flag cf CfPublic) && String.length cf.cf_name > 4 then begin match String.sub cf.cf_name 0 4 with
			| "get_" | "set_" -> false
			| _ -> can_access ctx c cf stat
		end else
			(not stat || not (has_class_field_flag cf CfImpl)) &&
			can_access ctx c cf stat
	in
	let make_class_field origin cf =
		let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta cf.cf_meta) cf.cf_type in
		make_ci_class_field (CompletionClassField.make cf CFSMember origin true) (cf.cf_type,ct)
	in
	let rec loop items t =
		let is_new_item items name = not (PMap.mem name items) in
		let rec browse_interfaces c acc =
			List.fold_left (fun acc (c,tl) ->
				let acc = List.fold_left (fun acc cf ->
					if is_new_item acc cf.cf_name then begin
						let origin = Parent(TClassDecl c) in
						let item = make_class_field origin cf in
						PMap.add cf.cf_name item acc
					end else
						acc
				) acc c.cl_ordered_fields in
				List.fold_left (fun acc (c,_) -> browse_interfaces c acc) acc c.cl_implements
			) acc c.cl_implements
		in
		match follow t with
		| TMono m ->
			let rec fold_constraints items = function
			| CStructural(fields,is_open) ->
				if not is_open then begin
					Monomorph.close m;
					begin match m.tm_type with
					| None -> items
					| Some t -> loop items t
					end
				end else
					loop items (mk_anon ~fields (ref Closed))
			| CTypes tl ->
				items
			| CUnknown ->
				items
			| CMixed l ->
				List.fold_left fold_constraints items l
			in
			fold_constraints items (Monomorph.classify_down_constraints m)
		| TInst ({cl_kind = KTypeParameter tl},_) ->
			(* Type parameters can access the fields of their constraints *)
			List.fold_left (fun acc t -> loop acc t) items tl
		| TInst(c0,tl) ->
			(* For classes, browse the hierarchy *)
			let fields = TClass.get_all_fields c0 tl in
			Display.merge_core_doc ctx (TClassDecl c0);
			let acc = PMap.foldi (fun k (c,cf) acc ->
				if should_access c cf false && is_new_item acc cf.cf_name then begin
					let origin = if c == c0 then Self(TClassDecl c) else Parent(TClassDecl c) in
					let item = make_class_field origin cf in
					PMap.add k item acc
				end else
					acc
			) fields items in
			let acc = if has_class_flag c0 CExtern && Meta.has Meta.LibType c0.cl_meta then
				browse_interfaces c0 acc
			else
				acc
			in
			acc
		| TEnum _ ->
			let t = ctx.g.do_load_type_def ctx p {tpackage=[];tname="EnumValue";tsub=None;tparams=[]} in
			begin match t with
			| TAbstractDecl ({a_impl = Some c} as a) ->
				begin try
					let cf = PMap.find "match" c.cl_statics in
					let item = make_class_field (Self(TAbstractDecl a)) cf in
					PMap.add "match" item items
				with Not_found ->
					items
				end
			| _ ->
				items
			end;
		| TAbstract({a_impl = Some c} as a,tl) ->
			Display.merge_core_doc ctx (TAbstractDecl a);
			(* Abstracts should show all their @:impl fields minus the constructor. *)
			let items = List.fold_left (fun acc cf ->
				if has_class_field_flag cf CfImpl && not (has_class_field_flag cf CfEnum) && should_access c cf false && is_new_item acc cf.cf_name then begin
					let origin = Self(TAbstractDecl a) in
					let cf = prepare_using_field cf in
					let cf = if tl = [] then cf else {cf with cf_type = apply_params a.a_params tl cf.cf_type} in
					let item = make_class_field origin cf in
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
				let abstract_has_own_field field_name =
					PMap.mem field_name c.cl_fields || PMap.mem field_name c.cl_statics
				in
				PMap.foldi (fun name item acc ->
					if (sl = [] || List.mem name sl && is_new_item acc name) && not (abstract_has_own_field name) then
						PMap.add name item acc
					else
						acc
				) forwarded_fields items
			with Not_found ->
				items
			end
		| TAnon an ->
			(* @:forwardStatics *)
			let items = match !(an.a_status) with
				| Statics { cl_kind = KAbstractImpl { a_meta = meta; a_this = TInst (c,_) }} when Meta.has Meta.ForwardStatics meta ->
					let items = List.fold_left (fun acc cf ->
						if should_access c cf true && is_new_item acc cf.cf_name then begin
							let origin = Self(TClassDecl c) in
							let item = make_class_field origin cf in
							PMap.add cf.cf_name item acc
						end else
							acc
					) items c.cl_ordered_statics in
					PMap.foldi (fun name item acc ->
						if is_new_item acc name then
							PMap.add name item acc
						else
							acc
					) PMap.empty items
				| _ -> items
			in
			(* Anon own fields *)
			PMap.foldi (fun name cf acc ->
				if is_new_item acc name then begin
					let allow_static_abstract_access c cf =
						should_access c cf false &&
						(not (has_class_field_flag cf CfImpl) || has_class_field_flag cf CfEnum)
					in
					let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta cf.cf_meta) cf.cf_type in
					let add origin make_field =
						PMap.add name (make_field (CompletionClassField.make cf CFSMember origin true) (cf.cf_type,ct)) acc
					in
					match !(an.a_status) with
						| Statics ({cl_kind = KAbstractImpl a} as c) ->
							if allow_static_abstract_access c cf then
								let make = if has_class_field_flag cf CfEnum then
										(make_ci_enum_abstract_field a)
									else
										make_ci_class_field
								in
								add (Self (TAbstractDecl a)) make
							else
								acc;
						| Statics c ->
							Display.merge_core_doc ctx (TClassDecl c);
							if should_access c cf true then add (Self (TClassDecl c)) make_ci_class_field else acc;
						| EnumStatics en ->
							let ef = PMap.find name en.e_constrs in
							PMap.add name (make_ci_enum_field (CompletionEnumField.make ef (Self (TEnumDecl en)) true) (cf.cf_type,ct)) acc
						| AbstractStatics a ->
							Display.merge_core_doc ctx (TAbstractDecl a);
							let check = match a.a_impl with
								| None -> true
								| Some c -> allow_static_abstract_access c cf
							in
							if check then add (Self (TAbstractDecl a)) make_ci_class_field else acc;
						| _ ->
							let origin = match t with
								| TType(td,_) -> Self (TTypeDecl td)
								| _ -> AnonymousStructure an
							in
							add origin make_ci_class_field;
				end else
					acc
			) an.a_fields items
		| TFun (args,ret) ->
			(* A function has no field except the magic .bind one. *)
			if is_new_item items "bind" then begin
				let t = opt_args args ret in
				let cf = mk_field "bind" (tfun [t] t) p null_pos in
				cf.cf_kind <- Method MethNormal;
				let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta cf.cf_meta) t in
				let item = make_ci_class_field (CompletionClassField.make cf CFSStatic BuiltIn true) (t,ct) in
				PMap.add "bind" item items
			end else
				items
		| _ ->
			items
	in
	(* Add special `.code` field if we have a string of length 1 *)
	let items = match fst e_ast with
		| EConst(String(s,_)) when String.length s = 1 ->
			let cf = mk_field "code" ctx.t.tint e.epos null_pos in
			cf.cf_doc <- doc_from_string "The character code of this character (inlined at compile-time).";
			cf.cf_kind <- Var { v_read = AccNormal; v_write = AccNever };
			let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta cf.cf_meta) cf.cf_type in
			let item = make_ci_class_field (CompletionClassField.make cf CFSMember BuiltIn true) (cf.cf_type,ct) in
			PMap.add cf.cf_name item PMap.empty
		| _ ->
			PMap.empty
	in
	(* Collect fields of the type *)
	let items = loop items e.etype in
	(* Add static extensions *)
	let items = collect_static_extensions ctx items e p in
	let items = PMap.fold (fun item acc -> item :: acc) items [] in
	let items = sort_fields items WithType.value (TKField p) in
	try
		let sl = string_list_of_expr_path_raise e_ast in
		(* Add submodule fields *)
		items @ get_submodule_fields ctx (List.tl sl,List.hd sl)
	with Exit | Not_found ->
		items

let handle_missing_field_raise ctx tthis i mode with_type pfield =
	let tret = match with_type with
		| WithType.WithType(t,_) -> t
		| WithType.Value _ -> mk_mono()
		| WithType.NoValue ->
			match mode with
			| MCall _ -> ctx.t.tvoid
			| MSet (Some e) ->
				begin try
					let e = type_expr ctx e WithType.value in
					e.etype
				with _ ->
					raise Exit
				end
			| _ -> raise Exit
	in
	let t,kind = match mode with
		| MCall el ->
			begin try
				let tl = List.mapi (fun i e ->
					let name = match Expr.find_ident e with
						| Some name -> name
						| None -> Printf.sprintf "arg%i" i
					in
					let e = type_expr ctx e WithType.value in
					(name,false,e.etype)
				) el in
				(TFun(tl,tret),Method MethNormal)
			with _ ->
				raise Exit
			end
		| MGet ->
			tret,Var {v_read = AccNormal;v_write = AccNo}
		| MSet _ ->
			tret,Var {v_read = AccNormal;v_write = AccNormal}
	in
	let cf = mk_field ~public:false i t pfield null_pos in
	cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos;Meta.NoCompletion,[],null_pos];
	cf.cf_kind <- kind;
	let mt,scope,public = match follow tthis with
		| TInst(c,_) -> TClassDecl c,CFSMember,not (can_access ctx c cf false)
		| TEnum(en,_) -> TEnumDecl en,CFSMember,true
		| TAbstract(a,_) -> TAbstractDecl a,CFSMember,true
		| TAnon an ->
			begin match !(an.a_status) with
			| Statics c -> TClassDecl c,CFSStatic,not (can_access ctx c cf true)
			| EnumStatics en -> TEnumDecl en,CFSStatic,true
			| AbstractStatics a -> TAbstractDecl a,CFSStatic,true
			| _ -> raise Exit
			end
		| _ ->
			raise Exit
	in
	if public then add_class_field_flag cf CfPublic;
	begin match scope with
		| CFSStatic -> add_class_field_flag cf CfStatic
		| _ -> ()
	end;
	let diag = {
		mf_pos = pfield;
		mf_on = mt;
		mf_fields = [(cf,t,CompletionItem.CompletionType.from_type (Display.get_import_status ctx) t)];
		mf_cause = FieldAccess;
	} in
	let display = ctx.com.display_information in
	display.module_diagnostics <- MissingFields diag :: display.module_diagnostics

let handle_missing_ident ctx i mode with_type p =
	match ctx.curfun with
	| FunStatic ->
		let e_self = Texpr.Builder.make_static_this ctx.curclass p in
		begin try
			handle_missing_field_raise ctx e_self.etype i mode with_type p
		with Exit ->
			()
		end
	| _ ->
		begin try
			handle_missing_field_raise ctx ctx.tthis i mode with_type p
		with Exit ->
			()
		end