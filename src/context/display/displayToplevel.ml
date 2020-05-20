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
open Common
open CompilationServer
open Type
open Typecore
open CompletionItem
open ClassFieldOrigin
open DisplayTypes
open Genjson
open Globals

let maybe_resolve_macro_field ctx t c cf =
	try
		if cf.cf_kind <> Method MethMacro then raise Exit;
		let (tl,tr,c,cf) = ctx.g.do_load_macro ctx false c.cl_path cf.cf_name null_pos in
		(TFun(tl,tr)),c,cf
	with _ ->
		t,c,cf

let exclude : string list ref = ref []

class explore_class_path_task cs com recursive f_pack f_module dir pack = object(self)
	inherit server_task ["explore";dir] 50
	val platform_str = platform_name_macro com

	method private execute : unit =
		let dot_path = (String.concat "." (List.rev pack)) in
		if (List.mem dot_path !exclude) then
			()
		else try
			let entries = Sys.readdir dir in
			Array.iter (fun file ->
				match file with
					| "." | ".." ->
						()
					| _ when Sys.is_directory (dir ^ file) && file.[0] >= 'a' && file.[0] <= 'z' ->
						begin try
							begin match PMap.find file com.package_rules with
								| Forbidden | Remap _ -> ()
								| _ -> raise Not_found
							end
						with Not_found ->
							f_pack (List.rev pack,file);
							if recursive then begin
								let task = new explore_class_path_task cs com recursive f_pack f_module (dir ^ file ^ "/") (file :: pack) in
								begin match cs with
									| None -> task#run
									| Some cs' -> cs'#add_task task
								end
							end
						end
					| _ ->
						let l = String.length file in
						if l > 3 && String.sub file (l - 3) 3 = ".hx" then begin
							try
								let name =
									let name = String.sub file 0 (l - 3) in
									try
										let dot_pos = String.rindex name '.' in
										if platform_str = String.sub file dot_pos (String.length name - dot_pos) then
											String.sub file 0 dot_pos
										else
											raise Exit
									with Not_found -> name
								in
								let path = (List.rev pack,name) in
								let dot_path = if dot_path = "" then name else dot_path ^ "." ^ name in
								if (List.mem dot_path !exclude) then () else f_module (dir ^ file) path;
							with _ ->
								()
						end
			) entries;
		with Sys_error _ ->
			()

end

let explore_class_paths com timer class_paths recursive f_pack f_module =
	let cs = CompilationServer.get() in
	let t = Timer.timer (timer @ ["class path exploration"]) in
	let tasks = List.map (fun dir ->
		new explore_class_path_task cs com recursive f_pack f_module dir []
	) class_paths in
	begin match cs with
	| None -> List.iter (fun task -> task#run) tasks
	| Some cs -> List.iter (fun task -> cs#add_task task) tasks
	end;
	t()

let read_class_paths com timer =
	explore_class_paths com timer (List.filter ((<>) "") com.class_path) true (fun _ -> ()) (fun file path ->
		(* Don't parse the display file as that would maybe overwrite the content from stdin with the file contents. *)
		if not (DisplayPosition.display_position#is_in_file file) then begin
			let file,_,pack,_ = Display.parse_module' com path Globals.null_pos in
			match CompilationServer.get() with
			| Some cs when pack <> fst path ->
				let file_key = Path.UniqueKey.create file in
				(CommonCache.get_cache cs com)#remove_file_for_real file_key
			| _ ->
				()
		end
	)

let init_or_update_server cs com timer_name =
	let cc = CommonCache.get_cache cs com in
	if not cc#is_initialized then begin
		cc#set_initialized true;
		read_class_paths com timer_name
	end;
	(* Force executing all "explore" tasks here because we need their information. *)
	cs#run_tasks true (fun task -> match task#get_id with
		| "explore" :: _ -> true
		| _ -> false
	);
	(* Iterate all removed files of the current context. If they aren't part of the context again,
		re-parse them and remove them from c_removed_files. *)
	let removed_files = cc#get_removed_files in
	let removed_removed_files = DynArray.create () in
	Hashtbl.iter (fun file_key file_path ->
		DynArray.add removed_removed_files file_key;
		try
			ignore(cc#find_file file_key);
		with Not_found ->
			try ignore(TypeloadParse.parse_module_file com file_path null_pos) with _ -> ()
	) removed_files;
	DynArray.iter (Hashtbl.remove removed_files) removed_removed_files

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

(* +1 for each matching package part. 0 = no common part *)
let pack_similarity pack1 pack2 =
	let rec loop count pack1 pack2 = match pack1,pack2 with
		| [],[] -> count
		| (s1 :: pack1),(s2 :: pack2) when s1 = s2 -> loop (count + 1) pack1 pack2
		| _ -> count
	in
	loop 0 pack1 pack2

(* Returns `true` if `pack1` contains or is `pack2` *)
let pack_contains pack1 pack2 =
	let rec loop pack1 pack2 = match pack1,pack2 with
		| [],_ -> true
		| (s1 :: pack1),(s2 :: pack2) when s1 = s2 -> loop pack1 pack2
		| _ -> false
	in
	loop pack1 pack2

let is_pack_visible pack =
	not (List.exists (fun s -> String.length s > 0 && s.[0] = '_') pack)

let collect ctx tk with_type sort =
	let t = Timer.timer ["display";"toplevel"] in
	let cctx = CollectionContext.create ctx in
	let curpack = fst ctx.curclass.cl_path in
	(* Note: This checks for the explicit `ServerConfig.legacy_completion` setting instead of using
	   `is_legacy_completion com` because the latter is always false for the old protocol, yet we have
	   tests which assume advanced completion even in the old protocol. This means that we can only
	   use "legacy mode" in the new protocol. *)
	let is_legacy_completion = !ServerConfig.legacy_completion in
	let packages = Hashtbl.create 0 in
	let add_package path = Hashtbl.replace packages path true in

	let add item name = add_item cctx item name in

	let add_type mt =
		match mt with
		| TClassDecl {cl_kind = KAbstractImpl _} -> ()
		| _ ->
			let path = (t_infos mt).mt_path in
			let mname = snd (t_infos mt).mt_module.m_path in
			let path = if snd path = mname then path else (fst path @ [mname],snd path) in
			if not (path_exists cctx path) then begin
				Display.merge_core_doc ctx mt;
				let is = get_import_status cctx true path in
				if not (Meta.has Meta.NoCompletion (t_infos mt).mt_meta) then begin
					add (make_ci_type (CompletionModuleType.of_module_type mt) is None) (Some (snd path));
					add_path cctx path;
				end
			end
	in

	let process_decls pack name decls =
		let added_something = ref false in
		let add item name =
			added_something := true;
			add item name
		in
		let run () = List.iter (fun (d,p) ->
			begin try
				let tname,is_private,meta = match d with
					| EClass d -> fst d.d_name,List.mem HPrivate d.d_flags,d.d_meta
					| EEnum d -> fst d.d_name,List.mem EPrivate d.d_flags,d.d_meta
					| ETypedef d -> fst d.d_name,List.mem EPrivate d.d_flags,d.d_meta
					| EAbstract d -> fst d.d_name,List.mem AbPrivate d.d_flags,d.d_meta
					| _ -> raise Exit
				in
				let path = Path.full_dot_path pack name tname in
				if not (path_exists cctx path) && not is_private && not (Meta.has Meta.NoCompletion meta) then begin
					add_path cctx path;
					(* If we share a package, the module's main type shadows everything with the same name. *)
					let shadowing_name = if pack_contains pack curpack && tname = name then (Some name) else None in
					(* Also, this means we can access it as if it was imported (assuming it's not shadowed by something else. *)
					let is = get_import_status cctx (shadowing_name <> None) path in
					add (make_ci_type (CompletionModuleType.of_type_decl pack name (d,p)) is None) shadowing_name
				end
			with Exit ->
				()
			end
		) decls in
		if is_pack_visible pack then run();
		!added_something
	in

	(* Collection starts here *)

	let tpair ?(values=PMap.empty) t =
		let ct = CompletionType.from_type (Display.get_import_status ctx) ~values t in
		(t,ct)
	in
	begin match tk with
	| TKType | TKOverride -> ()
	| TKExpr p | TKPattern p | TKField p ->
		(* locals *)
		PMap.iter (fun _ v ->
			if not (is_gen_local v) then
				add (make_ci_local v (tpair ~values:(get_value_meta v.v_meta) v.v_type)) (Some v.v_name)
		) ctx.locals;

		let add_field scope origin cf =
			let origin,cf = match origin with
				| Self (TClassDecl c) ->
					let _,c,cf = maybe_resolve_macro_field ctx cf.cf_type c cf in
					Self (TClassDecl c),cf
				| StaticImport (TClassDecl c) ->
					let _,c,cf = maybe_resolve_macro_field ctx cf.cf_type c cf in
					StaticImport (TClassDecl c),cf
				| Parent (TClassDecl c) ->
					let _,c,cf = maybe_resolve_macro_field ctx cf.cf_type c cf in
					Parent (TClassDecl c),cf
				| StaticExtension (TClassDecl c) ->
					let _,c,cf = maybe_resolve_macro_field ctx cf.cf_type c cf in
					StaticExtension (TClassDecl c),cf
				| _ ->
					origin,cf
			in
			let is_qualified = is_qualified cctx cf.cf_name in
			add (make_ci_class_field (CompletionClassField.make cf scope origin is_qualified) (tpair ~values:(get_value_meta cf.cf_meta) cf.cf_type)) (Some cf.cf_name)
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
			| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta && not (path_exists cctx a.a_path) && ctx.curclass != c ->
				add_path cctx a.a_path;
				List.iter (fun cf ->
					let ccf = CompletionClassField.make cf CFSMember (Self (decl_of_class c)) true in
					if (Meta.has Meta.Enum cf.cf_meta) && not (Meta.has Meta.NoCompletion cf.cf_meta) then
						add (make_ci_enum_abstract_field a ccf (tpair cf.cf_type)) (Some cf.cf_name);
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
					add (make_ci_enum_field (CompletionEnumField.make ef origin is_qualified) (tpair ef.ef_type)) (Some ef.ef_name)
				) e.e_constrs;
			| _ ->
				()
		in
		List.iter enum_ctors ctx.m.curmod.m_types;
		List.iter enum_ctors (List.map fst ctx.m.module_types);

		(* enum constructors of expected type *)
		begin match with_type with
			| WithType.WithType(t,_) ->
				(try enum_ctors (module_type_of_type (follow t)) with Exit -> ())
			| _ -> ()
		end;

		(* imported globals *)
		PMap.iter (fun name (mt,s,_) ->
			try
				let is_qualified = is_qualified cctx name in
				let class_import c =
					let cf = PMap.find s c.cl_statics in
					let cf = if name = cf.cf_name then cf else {cf with cf_name = name} in
					let decl,make = match c.cl_kind with
						| KAbstractImpl a -> TAbstractDecl a,
							if Meta.has Meta.Enum cf.cf_meta then make_ci_enum_abstract_field a else make_ci_class_field
						| _ -> TClassDecl c,make_ci_class_field
					in
					let origin = StaticImport decl in
					add (make (CompletionClassField.make cf CFSStatic origin is_qualified) (tpair ~values:(get_value_meta cf.cf_meta) cf.cf_type)) (Some name)
				in
				match resolve_typedef mt with
					| TClassDecl c -> class_import c;
					| TEnumDecl en ->
						let ef = PMap.find s en.e_constrs in
						let ef = if name = ef.ef_name then ef else {ef with ef_name = name} in
						let origin = StaticImport (TEnumDecl en) in
						add (make_ci_enum_field (CompletionEnumField.make ef origin is_qualified) (tpair ef.ef_type)) (Some s)
					| TAbstractDecl {a_impl = Some c} -> class_import c;
					| _ -> raise Not_found
			with Not_found ->
				()
		) ctx.m.module_globals;

		(* literals *)
		add (make_ci_literal "null" (tpair t_dynamic)) (Some "null");
		add (make_ci_literal "true" (tpair ctx.com.basic.tbool)) (Some "true");
		add (make_ci_literal "false" (tpair ctx.com.basic.tbool)) (Some "false");
		begin match ctx.curfun with
			| FunMember | FunConstructor | FunMemberClassLocal ->
				let t = TInst(ctx.curclass,List.map snd ctx.curclass.cl_params) in
				add (make_ci_literal "this" (tpair t)) (Some "this");
				begin match ctx.curclass.cl_super with
					| Some(c,tl) -> add (make_ci_literal "super" (tpair (TInst(c,tl)))) (Some "super")
					| None -> ()
				end
			| _ ->
				()
		end;

		if not is_legacy_completion then begin
			(* keywords *)
			let kwds = [
				Function; Var; Final; If; Else; While; Do; For; Break; Return; Continue; Switch;
				Try; New; Throw; Untyped; Cast; Inline;
			] in
			List.iter (fun kwd -> add(make_ci_keyword kwd) (Some (s_keyword kwd))) kwds;

			(* builtins *)
			add (make_ci_literal "trace" (tpair (TFun(["value",false,t_dynamic],ctx.com.basic.tvoid)))) (Some "trace")
		end
	end;

	(* type params *)
	List.iter (fun (s,t) -> match follow t with
		| TInst(c,_) ->
			add (make_ci_type_param c (tpair t)) (Some (snd c.cl_path))
		| _ -> die "" __LOC__
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
		explore_class_paths ctx.com ["display";"toplevel"] class_paths true add_package (fun file path ->
			if not (path_exists cctx path) then begin
				let _,decls = Display.parse_module ctx path Globals.null_pos in
				ignore(process_decls (fst path) (snd path) decls)
			end
		)
	| Some cs ->
		(* online: iter context files *)
		init_or_update_server cs ctx.com ["display";"toplevel"];
		let cc = CommonCache.get_cache cs ctx.com in
		let files = cc#get_files in
		(* Sort files by reverse distance of their package to our current package. *)
		let files = Hashtbl.fold (fun file cfile acc ->
			let i = pack_similarity curpack cfile.c_package in
			((file,cfile),i) :: acc
		) files [] in
		let files = List.sort (fun (_,i1) (_,i2) -> -compare i1 i2) files in
		let check_package pack = match List.rev pack with
			| [] -> ()
			| s :: sl -> add_package (List.rev sl,s)
		in
		List.iter (fun ((file_key,cfile),_) ->
			let module_name = CompilationServer.get_module_name_of_cfile cfile.c_file_path cfile in
			let dot_path = s_type_path (cfile.c_package,module_name) in
			(* In legacy mode we only show toplevel types. *)
			if is_legacy_completion && cfile.c_package <> [] then begin
				(* And only toplevel packages. *)
				match cfile.c_package with
				| [s] -> add_package ([],s)
				| _ -> ()
			end else if (List.exists (fun e -> ExtString.String.starts_with dot_path (e ^ ".")) !exclude) then
				()
			else begin
				Hashtbl.replace ctx.com.module_to_file (cfile.c_package,module_name) cfile.c_file_path;
				if process_decls cfile.c_package module_name cfile.c_decls then check_package cfile.c_package;
			end
		) files;
		List.iter (fun file ->
			match cs#get_native_lib file with
			| Some lib ->
				Hashtbl.iter (fun path (pack,decls) ->
					if process_decls pack (snd path) decls then check_package pack;
				) lib.c_nl_files
			| None ->
				()
		) ctx.com.native_libs.all_libs
	end;

	(* packages *)
	Hashtbl.iter (fun path _ ->
		let full_pack = fst path @ [snd path] in
		if is_pack_visible full_pack then add (make_ci_package path []) (Some (snd path))
	) packages;

	(* sorting *)
	let l = DynArray.to_list cctx.items in
	let l = if is_legacy_completion then
		List.sort (fun item1 item2 -> compare (get_name item1) (get_name item2)) l
	else if sort then
		Display.sort_fields l with_type tk
	else
		l
	in
	t();
	l

let collect_and_raise ctx tk with_type cr (name,pname) pinsert =
	let fields = match !DisplayException.last_completion_pos with
	| Some p' when pname.pmin = p'.pmin ->
		Array.to_list (!DisplayException.last_completion_result)
	| _ ->
		collect ctx tk with_type (name = "")
	in
	DisplayException.raise_fields fields cr (make_subject (Some name) ~start_pos:(Some pname) pinsert)

let handle_unresolved_identifier ctx i p only_types =
	let l = collect ctx (if only_types then TKType else TKExpr p) NoValue false in
	let cl = List.map (fun it ->
		let s = CompletionItem.get_name it in
		let i = StringError.levenshtein i s in
		(s,it,i),i
	) l in
	let cl = List.sort (fun (_,c1) (_,c2) -> compare c1 c2) cl in
	let cl = StringError.filter_similar (fun (s,_,_) r -> r <= (min (String.length s) (String.length i)) / 3) cl in
	ctx.com.display_information.unresolved_identifiers <- (i,p,cl) :: ctx.com.display_information.unresolved_identifiers
