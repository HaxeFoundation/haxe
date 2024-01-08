open Globals
open Type

type t = {
	platform : platform;
	defines : Define.define;
	basic : basic_types;
	debug : bool;
	file : string;
	features : (string,bool) Hashtbl.t;
	modules : Type.module_def list;
	main : Type.texpr option;
	types : Type.module_type list;
	resources : (string,string) Hashtbl.t;
	main_class : path option;
	native_libs : NativeLibraries.native_library_base list;
}

let raw_defined gctx v =
	Define.raw_defined gctx.defines v

let has_dce gctx =
	try
		Define.defined_value gctx.defines Define.Dce <> "no"
with Not_found ->
	false

let rec has_feature gctx f =
	try
		Hashtbl.find gctx.features f
	with Not_found ->
		if gctx.types = [] then not (has_dce gctx) else
		match List.rev (ExtString.String.nsplit f ".") with
		| [] -> die "" __LOC__
		| [cl] -> has_feature gctx (cl ^ ".*")
		| field :: cl :: pack ->
			let r = (try
				let path = List.rev pack, cl in
				(match List.find (fun t -> t_path t = path && not (Meta.has Meta.RealPath (t_infos t).mt_meta)) gctx.types with
				| t when field = "*" ->
					not (has_dce gctx) ||
					(match t with TAbstractDecl a -> Meta.has Meta.ValueUsed a.a_meta | _ -> Meta.has Meta.Used (t_infos t).mt_meta)
				| TClassDecl c when (has_class_flag c CExtern) && (gctx.platform <> Js || cl <> "Array" && cl <> "Math") ->
					not (has_dce gctx) || Meta.has Meta.Used (try PMap.find field c.cl_statics with Not_found -> PMap.find field c.cl_fields).cf_meta
				| TClassDecl c ->
					PMap.exists field c.cl_statics || PMap.exists field c.cl_fields
				| _ ->
					false)
			with Not_found ->
				false
			) in
			Hashtbl.add gctx.features f r;
			r

let get_entry_point gctx =
	Option.map (fun path ->
		let m = List.find (fun m -> m.m_path = path) gctx.modules in
		let c =
			match m.m_statics with
			| Some c when (PMap.mem "main" c.cl_statics) -> c
			| _ -> Option.get (ExtList.List.find_map (fun t -> match t with TClassDecl c when c.cl_path = path -> Some c | _ -> None) m.m_types)
		in
		let e = Option.get gctx.main in (* must be present at this point *)
		(snd path, c, e)
	) gctx.main_class