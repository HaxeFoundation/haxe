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
open Common
open Type
open Typecore
open DisplayTypes.CompletionKind
open DisplayTypes.CompletionItemKind

let explore_class_paths ctx class_paths recusive f_pack f_module =
	let rec loop dir pack =
		try
			let entries = Sys.readdir dir in
			Array.iter (fun file ->
				match file with
					| "." | ".." ->
						()
					| _ when Sys.is_directory (dir ^ file) && file.[0] >= 'a' && file.[0] <= 'z' ->
						begin try
							begin match PMap.find file ctx.com.package_rules with
								| Forbidden -> ()
								| _ -> raise Not_found
							end
						with Not_found ->
							f_pack file;
							if recusive then loop (dir ^ file ^ "/") (file :: pack)
						end
					| _ ->
						let l = String.length file in
						if l > 3 && String.sub file (l - 3) 3 = ".hx" then begin
							try
								let name = String.sub file 0 (l - 3) in
								let path = (List.rev pack,name) in
								f_module path;
							with _ ->
								()
						end
			) entries;
		with Sys_error _ ->
			()
	in
	List.iter (fun dir -> loop dir []) class_paths

let collect ctx only_types with_type =
	let acc = DynArray.create () in
	let add x = DynArray.add acc x in

	if not only_types then begin
		(* locals *)
		PMap.iter (fun _ v ->
			if not (is_gen_local v) then
				add (ITLocal v)
		) ctx.locals;

		(* member vars *)
		if ctx.curfun <> FunStatic then begin
			let rec loop c =
				List.iter (fun cf ->
					if not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITClassMember cf)
				) c.cl_ordered_fields;
				match c.cl_super with
					| None ->
						()
					| Some (csup,tl) ->
						loop csup; (* TODO: type parameters *)
			in
			loop ctx.curclass;
			(* TODO: local using? *)
		end;

		(* statics *)
		List.iter (fun cf ->
			if not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITClassStatic cf)
		) ctx.curclass.cl_ordered_statics;

		(* enum constructors *)
		let seen_paths = Hashtbl.create 0 in
		let was_proccessed path = Hashtbl.mem seen_paths path in
		let mark_processed path = Hashtbl.add seen_paths path true in
		let rec enum_ctors t =
			match t with
			| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta && not (was_proccessed a.a_path) ->
				mark_processed a.a_path;
				List.iter (fun cf ->
					if (Meta.has Meta.Enum cf.cf_meta) && not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITEnumAbstractField(a,cf));
				) c.cl_ordered_statics
			| TTypeDecl t ->
				begin match follow t.t_type with
					| TEnum (e,_) -> enum_ctors (TEnumDecl e)
					| _ -> ()
				end
			| TEnumDecl e when not (was_proccessed e.e_path) ->
				mark_processed e.e_path;
				PMap.iter (fun _ ef ->
					add (ITEnumField(e,ef))
				) e.e_constrs;
			| _ ->
				()
		in
		List.iter enum_ctors ctx.m.curmod.m_types;
		List.iter enum_ctors (List.map fst ctx.m.module_types);

		begin match with_type with
			| WithType t ->
				(try enum_ctors (module_type_of_type t) with Exit -> ())
			| _ -> ()
		end;

		(* imported globals *)
		PMap.iter (fun _ (mt,s,_) ->
			try
				let t = match resolve_typedef mt with
					| TClassDecl c -> (PMap.find s c.cl_statics).cf_type
					| TEnumDecl en -> (PMap.find s en.e_constrs).ef_type
					| TAbstractDecl {a_impl = Some c} -> (PMap.find s c.cl_statics).cf_type
					| _ -> raise Not_found
				in
				add (ITGlobal(mt,s,t))
			with Not_found ->
				()
		) ctx.m.module_globals;

		(* literals *)
		add (ITLiteral("null",t_dynamic));
		add (ITLiteral("true",ctx.com.basic.tbool));
		add (ITLiteral("false",ctx.com.basic.tbool));
		begin match ctx.curfun with
			| FunMember | FunConstructor | FunMemberClassLocal ->
				let t = TInst(ctx.curclass,List.map snd ctx.curclass.cl_params) in
				add (ITLiteral("this",t));
				begin match ctx.curclass.cl_super with
					| Some(c,tl) -> add (ITLiteral("super",TInst(c,tl)))
					| None -> ()
				end
			| _ ->
				()
		end
	end;

	let module_types = ref [] in

	let module_exists path =
		List.exists (fun (mt2,_) -> (t_infos mt2).mt_path = path) !module_types
	in

	let add_type rm mt =
		match mt with
		| TClassDecl {cl_kind = KAbstractImpl _} -> ()
		| _ ->
			let path = (t_infos mt).mt_path in
			if not (module_exists path) then begin
				(match mt with
				| TClassDecl c | TAbstractDecl { a_impl = Some c } when Meta.has Meta.CoreApi c.cl_meta ->
					!merge_core_doc_ref ctx c
				| _ -> ());
				module_types := (mt,rm) :: !module_types
			end
	in

	(* module types *)
	List.iter (add_type RMLocalModule) ctx.m.curmod.m_types;

	(* module imports *)
	List.iter (add_type RMImport) (List.map fst ctx.m.module_types);

	(* module using *)
	List.iter (fun (c,_) ->
		add_type RMUsing (TClassDecl c)
	) ctx.m.module_using;

	let packages = Hashtbl.create 0 in
	let add_package s = Hashtbl.replace packages s true in

	let class_paths = ctx.com.class_path in
	let class_paths = List.filter (fun s -> s <> "") class_paths in

	let add_syntax_type path kind rm =
		if not (module_exists path) then add (ITType(path,kind,rm))
	in

	let process_decls pack decls =
		List.iter (fun (d,_) -> match d with
			| EClass d when not (List.mem HPrivate d.d_flags) ->
				let kind = if List.mem HInterface d.d_flags then Interface else Class in
				add_syntax_type (pack,fst d.d_name) kind (RMOtherModule (pack,fst d.d_name)) (* TODO *)
			| EEnum d when not (List.mem EPrivate d.d_flags) ->
				add_syntax_type (pack,fst d.d_name) Enum (RMOtherModule (pack,fst d.d_name)) (* TODO *)
			| EAbstract d when not (List.mem AbPrivate d.d_flags) ->
				let kind = if Meta.has Meta.Enum d.d_meta then Enum else Class in
				add_syntax_type (pack,fst d.d_name) kind (RMOtherModule (pack,fst d.d_name)) (* TODO *)
			| ETypedef d when not (List.mem EPrivate d.d_flags) ->
				let kind = match fst d.d_data with
				| CTAnonymous _ -> Struct
				| _ -> Interface
				in
				add_syntax_type (pack,fst d.d_name) kind (RMOtherModule (pack,fst d.d_name)) (* TODO *)
			| _ -> ()
		) decls
	in

	begin match !CompilationServer.instance with
	| None ->
		explore_class_paths ctx class_paths true add_package (fun path ->
			if not (module_exists path) then begin
				let _,decls = TypeloadParse.parse_module ctx path Globals.null_pos in
				process_decls (fst path) decls
			end
		)
	| Some cs ->
		if not (CompilationServer.is_initialized cs) then begin
			CompilationServer.set_initialized cs;
			explore_class_paths ctx class_paths true (fun _ -> ()) (fun path ->
				if not (module_exists path) then begin
					ignore(TypeloadParse.parse_module ctx path Globals.null_pos)
				end
			);
		end;
		CompilationServer.iter_files cs ctx.com (fun (_,(pack,decls)) -> process_decls pack decls)
	end;

	(* TODO: wildcard packages. How? *)

	List.iter (fun (mt,rm) ->
		let kind = of_module_type mt in
		add (ITType((t_infos mt).mt_path,kind,rm))
	) !module_types;

	Hashtbl.iter (fun pack _ ->
		add (ITPackage pack)
	) packages;

	(* type params *)
	List.iter (fun (s,_) ->
		add (ITType (([],s),TypeParameter,RMTypeParameter))
	) ctx.type_params;

	let l = DynArray.to_list acc in
	match with_type with
		| WithType t ->
			let rec comp t' =
				if type_iseq t' t then 0 (* equal types - perfect *)
				else if t' == t_dynamic then 5 (* dynamic isn't good, but better than incompatible *)
				else try Type.unify t' t; 1 (* assignable - great *)
				with Unify_error _ -> match follow t' with
					| TFun(_,tr) ->
						if type_iseq tr t then 2 (* function returns our exact type - alright *)
						else (try Type.unify tr t; 3 (* function returns compatible type - okay *)
						with Unify_error _ -> 7) (* incompatible function - useless *)
					| _ ->
						6 (* incompatible type - probably useless *)
			in
			let l = List.map (fun ck -> ck,(comp (get_type ck),get_name ck)) l in
			let l = List.sort (fun (_,i1) (_,i2) -> compare i1 i2) l in
			List.map fst l
		| _ -> l

let handle_unresolved_identifier ctx i p only_types =
	let l = collect ctx only_types NoValue in
	let cl = List.map (fun it ->
		let s = DisplayTypes.CompletionKind.get_name it in
		(s,it),StringError.levenshtein i s
	) l in
	let cl = List.sort (fun (_,c1) (_,c2) -> compare c1 c2) cl in
	let cl = StringError.filter_similar (fun (s,_) r -> r > 0 && r <= (min (String.length s) (String.length i)) / 3) cl in
	ctx.com.display_information.unresolved_identifiers <- (i,p,cl) :: ctx.com.display_information.unresolved_identifiers
