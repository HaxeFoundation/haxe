open Globals
open Ast
open Common
open Type
open Error
open TyperBase
open Typecore

(* ---------------------------------------------------------------------- *)
(* FINALIZATION *)

let get_main ctx types =
	match ctx.com.main_class with
	| None -> None
	| Some path ->
		let p = null_pos in
		let pack,name = path in
		let m = Typeload.load_module ctx (pack,name) p in
		let c,f =
			let p = ref p in
			try
				match m.m_statics with
				| None ->
					raise Not_found
				| Some c ->
					p := c.cl_pos;
					c, PMap.find "main" c.cl_statics
			with Not_found -> try
				let t = Typeload.find_type_in_module_raise ctx m name null_pos in
				match t with
				| TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ ->
					error ("Invalid -main : " ^ s_type_path path ^ " is not a class") null_pos
				| TClassDecl c ->
					p := c.cl_pos;
					c, PMap.find "main" c.cl_statics
			with Not_found ->
				error ("Invalid -main : " ^ s_type_path path ^ " does not have static function main") !p
		in
		let ft = Type.field_type f in
		let fmode, r =
			match follow ft with
			| TFun ([],r) -> FStatic (c,f), r
			| _ -> error ("Invalid -main : " ^ s_type_path path ^ " has invalid main function") c.cl_pos
		in
		if not (ExtType.is_void (follow r)) then error (Printf.sprintf "Return type of main function should be Void (found %s)" (s_type (print_context()) r)) f.cf_name_pos;
		let emain = type_module_type ctx (TClassDecl c) None null_pos in
		let main = mk (TCall (mk (TField (emain,fmode)) ft null_pos,[])) r null_pos in
		(* add haxe.EntryPoint.run() call *)
		let add_entry_point_run exprs_rev =
			(try
				if ctx.com.config.pf_supports_threads then
					raise Not_found; (* Threaded targets run event loops per thread*)
				let et = List.find (fun t -> t_path t = (["haxe"],"EntryPoint")) types in
				let ec = (match et with TClassDecl c -> c | _ -> die "" __LOC__) in
				let ef = PMap.find "run" ec.cl_statics in
				let p = null_pos in
				let et = mk (TTypeExpr et) (mk_anon (ref (Statics ec))) p in
				let call = mk (TCall (mk (TField (et,FStatic (ec,ef))) ef.cf_type p,[])) ctx.t.tvoid p in
				call :: exprs_rev
			with Not_found ->
				exprs_rev
			)
		(* add sys.thread.Thread.processEvents() call *)
		and add_event_loop exprs_rev =
			(try
				let et = List.find (fun t -> t_path t = (["sys";"thread";"_Thread"],"Thread_Impl_")) types in
				let ec = (match et with TClassDecl c -> c | _ -> die "" __LOC__) in
				let ef = PMap.find "processEvents" ec.cl_statics in
				let p = null_pos in
				let et = mk (TTypeExpr et) (mk_anon (ref (Statics ec))) p in
				let call = mk (TCall (mk (TField (et,FStatic (ec,ef))) ef.cf_type p,[])) ctx.t.tvoid p in
				call :: exprs_rev
			with Not_found ->
				exprs_rev
			)
		in
		let main =
			match add_event_loop (add_entry_point_run [main]) with
			| [e] -> e
			| exprs_rev -> mk (TBlock (List.rev exprs_rev)) ctx.t.tvoid p
		in
		Some main

let finalize ctx =
	flush_pass ctx PFinal "final";
	match ctx.com.callbacks#get_after_typing with
		| [] ->
			()
		| fl ->
			let rec loop handled_types =
				let all_types = Hashtbl.fold (fun _ m acc -> m.m_types @ acc) ctx.g.modules [] in
				match (List.filter (fun mt -> not (List.memq mt handled_types)) all_types) with
				| [] ->
					()
				| new_types ->
					List.iter (fun f -> f new_types) fl;
					flush_pass ctx PFinal "final";
					loop all_types
			in
			loop []

type state =
	| Generating
	| Done
	| NotYet

let sort_types com modules =
	let types = ref [] in
	let states = Hashtbl.create 0 in
	let state p = try Hashtbl.find states p with Not_found -> NotYet in
	let statics = ref PMap.empty in

	let rec loop t =
		let p = t_path t in
		match state p with
		| Done -> ()
		| Generating ->
			com.warning ("Warning : maybe loop in static generation of " ^ s_type_path p) (t_infos t).mt_pos;
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
		(match c.cl_init with
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
	let sorted_modules = List.sort (fun m1 m2 -> compare m1.m_path m2.m_path) (Hashtbl.fold (fun _ m acc -> m :: acc) modules []) in
	List.iter (fun m -> List.iter loop m.m_types) sorted_modules;
	List.rev !types, sorted_modules

let generate ctx =
	let types,modules = sort_types ctx.com ctx.g.modules in
	get_main ctx types,types,modules
