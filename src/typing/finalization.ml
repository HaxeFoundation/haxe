open Globals
open Common
open Type
open Error
open TyperBase
open Typecore

(* ---------------------------------------------------------------------- *)
(* FINALIZATION *)

let maybe_load_main tctx = match tctx.com.main.main_path with
	| Some path ->
		Some (Typeload.load_module tctx path null_pos)
	| None ->
		None

let maybe_add_entrypoint com =
	begin match com.main.main_expr with
	| None ->
		()
	| Some main ->
		try
			let path = (["haxe"],"EntryPoint") in
			let method_name = "run" in
			let et = List.find (fun t -> t_path t = path) com.types in
			let ec = (match et with TClassDecl c -> c | _ -> die "" __LOC__) in
			let ef = PMap.find method_name ec.cl_statics in
			let et = Texpr.Builder.make_typeexpr et null_pos in
			let e = mk (TCall (mk (TField (et,FStatic (ec,ef))) ef.cf_type null_pos,[])) com.basic.tvoid null_pos in
			let e = mk (TBlock [main;e]) com.basic.tvoid main.epos in
			com.main.main_expr <- Some e
		with Not_found ->
			()
	end

let get_main ctx main_module types =
	match main_module with
	| None ->
		None,None
	| Some main_module ->
		let p = null_pos in
		let path = main_module.m_path in
		let pack,name = path in
		let c,f =
			let p = ref p in
			try
				match main_module.m_statics with
				| None ->
					raise Not_found
				| Some c ->
					p := c.cl_name_pos;
					c, PMap.find "main" c.cl_statics
			with Not_found -> try
				let t = Typeload.find_type_in_module_raise ctx main_module name null_pos in
				match t with
				| TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ ->
					raise_typing_error ("Invalid -main : " ^ s_type_path path ^ " is not a class") null_pos
				| TClassDecl c ->
					p := c.cl_name_pos;
					c, PMap.find "main" c.cl_statics
			with Not_found ->
				raise_typing_error ("Invalid -main : " ^ s_type_path path ^ " does not have static function main") !p
		in
		let ft = Type.field_type f in
		let fmode, r =
			match follow ft with
			| TFun ([],r) -> FStatic (c,f), r
			| _ -> raise_typing_error ("Invalid -main : " ^ s_type_path path ^ " has invalid main function") c.cl_pos
		in
		if not (ExtType.is_void (follow r)) then raise_typing_error (Printf.sprintf "Return type of main function should be Void (found %s)" (s_type (print_context()) r)) f.cf_name_pos;
		if not (Meta.has Meta.Keep f.cf_meta) then f.cf_meta <- (Dce.mk_keep_meta f.cf_pos) :: f.cf_meta;
		let emain = type_module_type ctx (TClassDecl c) null_pos in
		let main = mk (TCall (mk (TField (emain,fmode)) ft null_pos,[])) r null_pos in
		Some main,Some (Path.UniqueKey.lazy_path main_module.m_extra.m_file)

let finalize ctx =
	run_stages ctx.g StageFinal ("final",[]);
	match ctx.com.callbacks#get_after_typing with
		| [] ->
			()
		| fl ->
			let rec loop handled_types =
				let all_types = ctx.com.module_lut#fold (fun _ m acc -> m.m_types @ acc) [] in
				match (List.filter (fun mt -> not (List.memq mt handled_types)) all_types) with
				| [] ->
					()
				| new_types ->
					List.iter (fun f -> f new_types) fl;
					run_stages ctx.g StageFinal ("final",[]);
					loop all_types
			in
			loop []

type state =
	| Generating
	| Done
	| NotYet

(* Orders the types of [sorted_modules] so that a type comes after the types its
   static initialization depends on. [keep] restricts the result to a set of
   types (e.g. after DCE). *)
let sort_modules_types com ?(keep=fun _ -> true) ?(warn=true) sorted_modules =
	let types = ref [] in
	let states = Hashtbl.create 0 in
	let state p = try Hashtbl.find states p with Not_found -> NotYet in
	let statics = ref PMap.empty in

	let rec loop t =
		let p = t_path t in
		if not (keep t) then () else
		match state p with
		| Done -> ()
		| Generating ->
			if warn then module_warning com (t_infos t).mt_module WStaticInitOrder [] ("Warning : maybe loop in static generation of " ^ s_type_path p) (t_infos t).mt_pos;
		| NotYet ->
			Hashtbl.add states p Generating;
			let t = (match t with
			| TClassDecl c ->
				walk_class p c;
				t
			| TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ ->
				t
			) in
			Hashtbl.replace states p Done;
			types := t :: !types

	and loop_class p c =
		if c.cl_path <> p then loop (TClassDecl c)

	and loop_enum p e =
		if e.e_path <> p then loop (TEnumDecl e)

	and loop_abstract p a =
		if a.a_path <> p then loop (TAbstractDecl a)

	and walk_static_field p c cf =
		match cf.cf_expr with
		| None -> ()
		| Some e ->
			if PMap.mem (c.cl_path,cf.cf_name) (!statics) then
				()
			else begin
				statics := PMap.add (c.cl_path,cf.cf_name) () (!statics);
				walk_expr p e;
			end

	and walk_expr p e =
		match e.eexpr with
		| TTypeExpr t ->
			(match t with
			| TClassDecl c -> loop_class p c
			| TEnumDecl e -> loop_enum p e
			| TAbstractDecl a -> loop_abstract p a
			| TTypeDecl _ -> die "" __LOC__)
		| TNew (c,_,_) ->
			iter (walk_expr p) e;
			loop_class p c;
			let rec loop c =
				if PMap.mem (c.cl_path,"new") (!statics) then
					()
				else begin
					statics := PMap.add (c.cl_path,"new") () !statics;
					(match c.cl_constructor with
					| Some { cf_expr = Some e } -> walk_expr p e
					| _ -> ());
					match c.cl_super with
					| None -> ()
					| Some (csup,_) -> loop csup
				end
			in
			loop c
		| TField(e1,FStatic(c,cf)) ->
			walk_expr p e1;
			walk_static_field p c cf;
		| _ ->
			iter (walk_expr p) e

	and walk_class p c =
		(match c.cl_super with None -> () | Some (c,_) -> loop_class p c);
		List.iter (fun (c,_) -> loop_class p c) c.cl_implements;
		(match TClass.get_cl_init c with
		| None -> ()
		| Some e -> walk_expr p e);
		PMap.iter (fun _ f ->
			match f.cf_expr with
			| None -> ()
			| Some e ->
				match e.eexpr with
				| TFunction _ -> ()
				| _ -> walk_expr p e
		) c.cl_statics

	in
	List.iter (fun m -> List.iter loop m.m_types) sorted_modules;
	List.rev !types

let sort_types com (modules : module_lut) =
	let sorted_modules = List.sort (fun m1 m2 -> compare m1.m_path m2.m_path) (modules#fold (fun _ m acc -> m :: acc) []) in
	sort_modules_types com sorted_modules, sorted_modules

(* Orders [types] again once post-processing is done. The order computed during
   typing depends on the expressions it walks, which may already be
   post-processed for modules reused from the compilation server's cache and
   not for the others; after the filters, every type has its post-processed
   expressions, so the order is the same whatever the cache state
   (docs/incremental-compilation.md: deterministic output order). *)
let sort_type_list com types =
	(* Types are identified physically, grouped by module: after the filters,
	   @:native paths can make two types share a path. *)
	let by_module = Hashtbl.create 0 in
	let modules = Hashtbl.create 0 in
	List.iter (fun t ->
		let m = (t_infos t).mt_module in
		Hashtbl.replace modules m.m_path m;
		Hashtbl.replace by_module m.m_path (t :: (try Hashtbl.find by_module m.m_path with Not_found -> []))
	) types;
	let mem t tbl =
		let infos = t_infos t in
		List.exists (fun t' -> t_infos t' == infos) (try Hashtbl.find tbl infos.mt_module.m_path with Not_found -> [])
	in
	let sorted_modules = List.sort (fun m1 m2 -> compare m1.m_path m2.m_path) (Hashtbl.fold (fun _ m acc -> m :: acc) modules []) in
	let sorted = sort_modules_types com ~keep:(fun t -> mem t by_module) ~warn:false sorted_modules in
	(* Types that are not in the type list of their module (if any) keep their
	   relative order, at the end. *)
	let sorted_set = Hashtbl.create (List.length sorted) in
	List.iter (fun t ->
		let k = (t_infos t).mt_module.m_path in
		Hashtbl.replace sorted_set k (t :: (try Hashtbl.find sorted_set k with Not_found -> []))
	) sorted;
	sorted @ List.filter (fun t -> not (mem t sorted_set)) types

let generate ctx main_path =
	let types,modules = sort_types ctx.com ctx.com.module_lut in
	get_main ctx main_path types,types,modules
