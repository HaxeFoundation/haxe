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
open CompilationServer
open Type
open Typecore
open CompletionItem
open ClassFieldOrigin
open DisplayTypes
open Genjson

let explore_class_paths com timer class_paths recusive f_pack f_module =
	let rec loop dir pack =
		try
			let entries = Sys.readdir dir in
			Array.iter (fun file ->
				match file with
					| "." | ".." ->
						()
					| _ when Sys.is_directory (dir ^ file) && file.[0] >= 'a' && file.[0] <= 'z' ->
						begin try
							begin match PMap.find file com.package_rules with
								| Forbidden -> ()
								| _ -> raise Not_found
							end
						with Not_found ->
							f_pack (List.rev pack,file);
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
	let t = Timer.timer (timer @ ["class path exploration"]) in
	List.iter (fun dir -> loop dir []) class_paths;
	t()

let read_class_paths com timer =
	let sign = Define.get_signature com.defines in
	explore_class_paths com timer (List.filter ((<>) "") com.class_path) true (fun _ -> ()) (fun path ->
		let file,_,pack,_ = TypeloadParse.parse_module' com path Globals.null_pos in
		match CompilationServer.get() with
		| Some cs when pack <> fst path ->
			let file = Path.unique_full_path file in
			CompilationServer.remove_file cs (file,sign)
		| _ ->
			()
	)

module CollectionContext = struct
	open ImportStatus

	type t = {
		ctx   : typer;
		items : CompletionItem.t DynArray.t;
		names : (string,CompletionItem.t) Hashtbl.t;
		paths : (Globals.path,bool) Hashtbl.t;
	}

	let create ctx = {
		ctx = ctx;
		items = DynArray.create ();
		names = Hashtbl.create 0;
		paths = Hashtbl.create 0;
	}

	let add_item ctx item name =
		DynArray.add ctx.items item;
		match name with
		| None -> ()
		| Some name -> Hashtbl.replace ctx.names name item

	let get_import_status ctx is_import path =
		try
			let _ = Hashtbl.find ctx.names (snd path) in
			(* TODO: do we have to check if we get the same thing? *)
			Shadowed
		with Not_found ->
			let check_wildcard () =
				List.exists (fun (sl,_) -> (sl,snd path) = path) ctx.ctx.m.wildcard_packages
			in
			if is_import || (fst path = []) || check_wildcard () then Imported else Unimported

	let is_qualified ctx name =
		not (Hashtbl.mem ctx.names name)

	let path_exists ctx path = Hashtbl.mem ctx.paths path
	let add_path ctx path = Hashtbl.add ctx.paths path true
end

open CollectionContext

let collect ctx only_types with_type =
	let t = Timer.timer ["display";"toplevel"] in
	let cctx = CollectionContext.create ctx in
	let packages = Hashtbl.create 0 in
	let add_package path = Hashtbl.replace packages path true in

	let add item name = add_item cctx item name in

	let add_type mt =
		match mt with
		| TClassDecl {cl_kind = KAbstractImpl _} -> ()
		| _ ->
			let path = (t_infos mt).mt_path in
			if not (path_exists cctx path) then begin
				(match mt with
				| TClassDecl c | TAbstractDecl { a_impl = Some c } when Meta.has Meta.CoreApi c.cl_meta ->
					!merge_core_doc_ref ctx c
				| _ -> ());
                let is = get_import_status cctx true path in
				add (make_ci_type (CompletionModuleType.of_module_type mt) is None) (Some (snd path));
				add_path cctx path;
			end
	in

	let process_decls pack name decls =
		let run () = List.iter (fun (d,p) ->
			begin try
				let tname,is_private = match d with
					| EClass d -> fst d.d_name,List.mem HPrivate d.d_flags
					| EEnum d -> fst d.d_name,List.mem EPrivate d.d_flags
					| ETypedef d -> fst d.d_name,List.mem EPrivate d.d_flags
					| EAbstract d -> fst d.d_name,List.mem AbPrivate d.d_flags
					| _ -> raise Exit
				in
				let path = (pack,tname) in
				if not (path_exists cctx path) && not is_private then begin
					add_path cctx path;
					let is = get_import_status cctx false path in
					add (make_ci_type (CompletionModuleType.of_type_decl pack name (d,p)) is None) None
				end
			with Exit ->
				()
			end
		) decls in
		if not (List.exists (fun s -> String.length s > 0 && s.[0] = '_') pack) then run()
	in

	(* Collection starts here *)

	if not only_types then begin
		(* locals *)
		PMap.iter (fun _ v ->
			if not (is_gen_local v) then
				add (make_ci_local v v.v_type) (Some v.v_name)
		) ctx.locals;

		let add_field scope origin cf =
			let is_qualified = is_qualified cctx cf.cf_name in
			add (make_ci_class_field (CompletionClassField.make cf scope origin is_qualified) cf.cf_type) (Some cf.cf_name)
		in
		let maybe_add_field scope origin cf =
			if not (Meta.has Meta.NoCompletion cf.cf_meta) then add_field scope origin cf
		in
		(* member fields *)
		if ctx.curfun <> FunStatic then begin
			let all_fields = Type.TClass.get_all_fields ctx.curclass (List.map snd ctx.curclass.cl_params) in
			PMap.iter (fun _ (c,cf) ->
				let origin = if c == ctx.curclass then Self (TClassDecl c) else Parent (TClassDecl c) in
				maybe_add_field CFSMember origin cf
			) all_fields;
			(* TODO: local using? *)
		end;

		(* statics *)
		begin match ctx.curclass.cl_kind with
		| KAbstractImpl ({a_impl = Some c} as a) ->
			let origin = Self (TAbstractDecl a) in
			List.iter (fun cf ->
				if Meta.has Meta.Impl cf.cf_meta then begin
					if ctx.curfun = FunStatic then ()
					else begin
						let cf = prepare_using_field cf in
						maybe_add_field CFSMember origin cf
					end
				end else
					maybe_add_field CFSStatic origin cf
			) c.cl_ordered_statics
		| _ ->
			List.iter (maybe_add_field CFSStatic (Self (TClassDecl ctx.curclass))) ctx.curclass.cl_ordered_statics
		end;

		(* enum constructors *)
		let rec enum_ctors t =
			match t with
			| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta && not (path_exists cctx a.a_path) ->
				add_path cctx a.a_path;
				List.iter (fun cf ->
					let ccf = CompletionClassField.make cf CFSMember (Self (TClassDecl c)) true in
					if (Meta.has Meta.Enum cf.cf_meta) && not (Meta.has Meta.NoCompletion cf.cf_meta) then
						add (make_ci_enum_abstract_field a ccf cf.cf_type) (Some cf.cf_name);
				) c.cl_ordered_statics
			| TTypeDecl t ->
				begin match follow t.t_type with
					| TEnum (e,_) -> enum_ctors (TEnumDecl e)
					| _ -> ()
				end
			| TEnumDecl e when not (path_exists cctx e.e_path) ->
				add_path cctx e.e_path;
				let origin = Self (TEnumDecl e) in
				PMap.iter (fun _ ef ->
					let is_qualified = is_qualified cctx ef.ef_name in
					add (make_ci_enum_field (CompletionEnumField.make ef origin is_qualified) ef.ef_type) (Some ef.ef_name)
				) e.e_constrs;
			| _ ->
				()
		in
		List.iter enum_ctors ctx.m.curmod.m_types;
		List.iter enum_ctors (List.map fst ctx.m.module_types);

		(* enum constructors of expected type *)
		begin match with_type with
			| WithType t ->
				(try enum_ctors (module_type_of_type t) with Exit -> ())
			| _ -> ()
		end;

		(* imported globals *)
		PMap.iter (fun name (mt,s,_) ->
			try
				let is_qualified = is_qualified cctx name in
				let class_import c =
					let cf = PMap.find s c.cl_statics in
					let cf = if name = cf.cf_name then cf else {cf with cf_name = name} in
					let origin = StaticImport (TClassDecl c) in
					add (make_ci_class_field (CompletionClassField.make cf CFSStatic origin is_qualified) cf.cf_type) (Some name)
				in
				match resolve_typedef mt with
					| TClassDecl c -> class_import c;
					| TEnumDecl en ->
						let ef = PMap.find s en.e_constrs in
						let ef = if name = ef.ef_name then ef else {ef with ef_name = name} in
						let origin = StaticImport (TEnumDecl en) in
						add (make_ci_enum_field (CompletionEnumField.make ef origin is_qualified) ef.ef_type) (Some s)
					| TAbstractDecl {a_impl = Some c} -> class_import c;
					| _ -> raise Not_found
			with Not_found ->
				()
		) ctx.m.module_globals;

		(* literals *)
		add (make_ci_literal "null" t_dynamic) (Some "null");
		add (make_ci_literal "true" ctx.com.basic.tbool) (Some "true");
		add (make_ci_literal "false" ctx.com.basic.tbool) (Some "false");
		begin match ctx.curfun with
			| FunMember | FunConstructor | FunMemberClassLocal ->
				let t = TInst(ctx.curclass,List.map snd ctx.curclass.cl_params) in
				add (make_ci_literal "this" t) (Some "this");
				begin match ctx.curclass.cl_super with
					| Some(c,tl) -> add (make_ci_literal "super" (TInst(c,tl))) (Some "super")
					| None -> ()
				end
			| _ ->
				()
		end;

		(* keywords *)
		let kwds = [
			Function; Var; If; Else; While; Do; For; Break; Return; Continue; Switch;
			Try; New; Throw; Untyped; Cast;
		] in
		List.iter (fun kwd -> add(make_ci_keyword kwd) (Some (s_keyword kwd))) kwds;

		(* builtins *)
		add (make_ci_literal "trace" (TFun(["value",false,t_dynamic],ctx.com.basic.tvoid))) (Some "trace")
	end;

	(* type params *)
	List.iter (fun (s,t) -> match follow t with
		| TInst(c,_) ->
			(* This is weird, might want to use something else for type parameters *)
			add (make_ci_type (CompletionModuleType.of_module_type (TClassDecl c)) ImportStatus.Imported (Some t)) (Some s)
		| _ -> assert false
	) ctx.type_params;

	(* module types *)
	List.iter add_type ctx.m.curmod.m_types;

	(* module imports *)
	List.iter add_type (List.rev_map fst ctx.m.module_types); (* reverse! *)

	(* types from files *)
	begin match !CompilationServer.instance with
	| None ->
		(* offline: explore class paths *)
		let class_paths = ctx.com.class_path in
		let class_paths = List.filter (fun s -> s <> "") class_paths in
		explore_class_paths ctx.com ["display";"toplevel"] class_paths true add_package (fun path ->
			if not (path_exists cctx path) then begin
				let _,decls = TypeloadParse.parse_module ctx path Globals.null_pos in
				process_decls (fst path) (snd path) decls
			end
		)
	| Some cs ->
		(* online: iter context files *)
		if not (CompilationServer.is_initialized cs) then begin
			CompilationServer.set_initialized cs;
			read_class_paths ctx.com ["display";"toplevel"];
		end;
		(* TODO: sort files so that the ones in the current module's package and its parent packages
		   are processed first. *)
		CompilationServer.iter_files cs ctx.com (fun file cfile ->
			let module_name = match cfile.c_module_name with
			| None ->
				let name = Path.module_name_of_file file in
				cfile.c_module_name <- Some name;
				name
			| Some name ->
				name
			in
			begin match List.rev cfile.c_package with
				| [] -> ()
				| s :: sl -> add_package (List.rev sl,s)
			end;
			Hashtbl.replace ctx.com.module_to_file (cfile.c_package,module_name) file;
			process_decls cfile.c_package module_name cfile.c_decls
		)
	end;

	Hashtbl.iter (fun path _ ->
		add (make_ci_package path []) (Some (snd path))
	) packages;

	(* sorting *)
	let l = DynArray.to_list cctx.items in
	let l = match with_type with
		| WithType t ->
			let rec comp t' = match t' with
				| None -> 9
				| Some t' ->
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
			let l = List.map (fun ck ->
				let s1 = comp (get_type ck) in
				let s2 = get_sort_index ck in
				let s3 = get_name ck in
				ck,(s1,s2,s3)
			) l in
			let l = List.sort (fun (_,i1) (_,i2) -> compare i1 i2) l in
			List.map fst l
		| _ -> l
	in
	t();
	l

let handle_unresolved_identifier ctx i p only_types =
	let l = collect ctx only_types NoValue in
	let cl = List.map (fun it ->
		let s = CompletionItem.get_name it in
		let i = StringError.levenshtein i s in
		(s,it,i),i
	) l in
	let cl = List.sort (fun (_,c1) (_,c2) -> compare c1 c2) cl in
	let cl = StringError.filter_similar (fun (s,_,_) r -> r <= (min (String.length s) (String.length i)) / 3) cl in
	ctx.com.display_information.unresolved_identifiers <- (i,p,cl) :: ctx.com.display_information.unresolved_identifiers
