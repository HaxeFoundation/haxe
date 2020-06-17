open Globals
open Ast
open ExtString
open NativeLibraries
open JData

type java_lib_ctx = {
	type_params : (string,complex_type) PMap.t;
}

let jname_to_hx name =
	let name =
		if name <> "" && (String.get name 0 < 'A' || String.get name 0 > 'Z') then
			Char.escaped (Char.uppercase (String.get name 0)) ^ String.sub name 1 (String.length name - 1)
		else
			name
	in
	let name = String.concat "__" (String.nsplit name "_") in
	match String.nsplit name "$" with
	| [] ->
		die "" __LOC__
	| [_] ->
		None,name
	| x :: l ->
		let name = String.concat "_" (x :: l) in
		if x = "" then None,name (* leading $ *)
		else Some x,name

let normalize_pack pack =
	List.map (function
		| "" -> ""
		| str when String.get str 0 >= 'A' && String.get str 0 <= 'Z' ->
			String.lowercase str
		| str -> str
	) pack

let jpath_to_hx (pack,name) =
	let pack,name = match pack,name with
	| ["haxe";"root"],name ->
		[],name
	| "com" :: ("oracle" | "sun") :: _, _
	| "javax" :: _, _
	| "org" :: ("ietf" | "jcp" | "omg" | "w3c" | "xml") :: _, _
	| "sun" :: _, _
	| "sunw" :: _, _ ->
		"java" :: pack,name
	| _ ->
		pack,name
	in
	let pack = normalize_pack pack in
	pack,jname_to_hx name

let jpath_to_path (pack,(mname,name)) =
	let pack,name = match mname with
		| None -> pack,name
		| Some mname -> pack @ [mname],name
	in
	pack,name

let is_haxe_keyword = function
	| "cast" | "extern" | "function" | "in" | "typedef" | "using" | "var" | "untyped" | "inline" -> true
	| _ -> false

let mk_type_path path params =
	let pack,(mname,name) = jpath_to_hx path in
	match mname with
	| None ->
		CTPath {
			tpackage = pack;
			tname = name;
			tparams = params;
			tsub = None;
		}
	| Some mname ->
		CTPath {
			tpackage = pack;
			tname = mname;
			tparams = params;
			tsub = Some name;
		}

let ct_type_param name = CTPath {
	tpackage = [];
	tname = name;
	tparams = [];
	tsub = None
}

let ct_void = CTPath {
	tpackage = [];
	tname = "Void";
	tparams = [];
	tsub = None;
}

let ct_dynamic = CTPath {
	tpackage = [];
	tname = "Dynamic";
	tparams = [];
	tsub = None;
}

let ct_string = CTPath {
	tpackage = [];
	tname = "String";
	tparams = [];
	tsub = None;
}

let rec convert_arg ctx p arg =
	match arg with
	| TAny | TType (WSuper, _) -> TPType (mk_type_path ([], "Dynamic") [],p)
	| TType (_, jsig) -> TPType (convert_signature ctx p jsig,p)

and convert_signature ctx p jsig =
	match jsig with
	| TByte -> mk_type_path (["java"; "types"], "Int8") []
	| TChar -> mk_type_path (["java"; "types"], "Char16") []
	| TDouble -> mk_type_path ([], "Float") []
	| TFloat -> mk_type_path ([], "Single") []
	| TInt -> mk_type_path ([], "Int") []
	| TLong -> mk_type_path (["haxe"], "Int64") []
	| TShort -> mk_type_path (["java"; "types"], "Int16") []
	| TBool -> mk_type_path ([], "Bool") []
	| TObject ( (["haxe";"root"], name), args ) -> mk_type_path ([], name) (List.map (convert_arg ctx p) args)
	| TObject ( (["java";"lang"], "Object"), [] ) -> mk_type_path ([], "Dynamic") []
	| TObject ( (["java";"lang"], "String"), [] ) -> mk_type_path ([], "String") []
	| TObject ( (["java";"lang"], "Enum"), [_] ) -> mk_type_path ([], "EnumValue") []
	| TObject ( path, [] ) ->
		mk_type_path path []
	| TObject ( path, args ) -> mk_type_path path (List.map (convert_arg ctx p) args)
	| TObjectInner (pack, (name, params) :: inners) ->
		let actual_param = match List.rev inners with
		| (_, p) :: _ -> p
		| _ -> die "" __LOC__ in
		mk_type_path (pack, name ^ "$" ^ String.concat "$" (List.map fst inners)) (List.map (fun param -> convert_arg ctx p param) actual_param)
	| TObjectInner (pack, inners) -> die "" __LOC__
	| TArray (jsig, _) -> mk_type_path (["java"], "NativeArray") [ TPType (convert_signature ctx p jsig,p) ]
	| TMethod _ -> JReader.error "TMethod cannot be converted directly into Complex Type"
	| TTypeParameter s ->
		try
			PMap.find s ctx.type_params
		with Not_found ->
			ct_dynamic

let get_type_path ct = match ct with | CTPath p -> p | _ -> die "" __LOC__

module Converter = struct

	let convert_type_parameter ctx (name,extends,implements) p =
		let jsigs = match extends with
			| Some jsig -> jsig :: implements
			| None -> implements
		in
		let constraints = ExtList.List.filter_map (fun jsig -> match jsig with
			| TTypeParameter name' when name = name' ->
				None
			| _ ->
				Some (convert_signature ctx p jsig,p)
		) jsigs in
		let tp = {
			tp_name = (name,p);
			tp_params = [];
			tp_meta = [];
			tp_constraints = match constraints with
				| [] -> None
				| _ -> Some (CTIntersection constraints,p)
		} in
		tp

	let convert_enum (jc : jclass) (file : string) =
		let p = {
			pfile = file;
			pmin = 0;
			pmax = 0
		} in
		let meta = ref [] in
		let add_meta m = meta := m :: !meta in
		let data = ref [] in
		List.iter (fun f ->
			match f.jf_vmsignature with
			| TObject( path, [] ) when path = jc.cpath && List.mem JStatic f.jf_flags && List.mem JFinal f.jf_flags ->
				data := { ec_name = f.jf_name,p; ec_doc = None; ec_meta = []; ec_args = []; ec_pos = p; ec_params = []; ec_type = None; } :: !data;
			| _ -> ()
		) jc.cfields;
		let _,class_name = jname_to_hx (snd jc.cpath) in
		add_meta (Meta.Native, [EConst (String (s_type_path jc.cpath,SDoubleQuotes) ),p],p);
		let d = {
			d_name = (class_name,p);
			d_doc = None;
			d_params = []; (* enums never have type parameters *)
			d_meta = !meta;
			d_flags = [EExtern];
			d_data = List.rev !data;
		} in
		(EEnum d,p)

	let type_param_lut acc params =
		List.fold_left (fun acc (s,_,_) ->
			PMap.add s (ct_type_param s) acc
		) acc params

	let convert_field ctx (jc : jclass) (jf : jfield) p =
		let ctx = {
			type_params = type_param_lut ctx.type_params jf.jf_types;
		} in
		let p = {p with pfile = p.pfile ^ "@" ^ jf.jf_name} in
		let is_static = List.mem JStatic jf.jf_flags in
		let access = ref [] in
		let meta = ref [] in
		let add_access a = access := a :: !access in
		let add_meta m = meta := m :: !meta in
		if is_static then add_access (AStatic,p);
		List.iter (function
			| AttrDeprecated when jc.cpath <> (["java";"util"],"Date") ->
				add_meta (Meta.Deprecated,[],p);
			| AttrVisibleAnnotations ann ->
				List.iter (function
					| { ann_type = TObject( (["java";"lang"], "Override"), [] ) } ->
						add_access (AOverride,null_pos);
					| _ -> ()
				) ann
			| _ -> ()
		) jf.jf_attributes;
		let add_native_meta () =
			add_meta (Meta.Native, [EConst (String (jf.jf_name,SDoubleQuotes) ),p],p)
		in
		let name = match String.nsplit jf.jf_name "$" with
			| ["<init>"] ->
				"new"
			| [name] ->
				if is_haxe_keyword name then begin
					add_native_meta();
					"_" ^ name
				end else
					name
			| parts ->
				add_native_meta();
				String.concat "_" parts
		in
		if jf.jf_kind = JKMethod then add_meta (Meta.Overload,[],p);
		if List.mem JFinal jf.jf_flags then add_access (AFinal,p);
		let extract_local_names () =
			let default i =
				"param" ^ string_of_int i
			in
			let rec loop attribs = match attribs with
				| AttrLocalVariableTable locals :: _ ->
					List.map (fun loc ->
						loc.ld_index,loc.ld_name
					) locals
				| AttrMethodParameters l :: _ ->
					List.mapi (fun i (name,_) ->
						(i,name)
					) l
				| _ :: attribs ->
					loop attribs
				| [] ->
					raise Not_found
			in
			let use locals =
				let h = Hashtbl.create 0 in
				List.iter (fun (index,name) ->
					Hashtbl.replace h index name
				) locals;
				(fun i ->
					try Hashtbl.find h (i - 1) (* they are 1-based *)
					with Not_found -> "param" ^ string_of_int i
				)
			in
			try
				use (loop jf.jf_attributes)
			with Not_found -> try
				match jf.jf_code with
				| None ->
					default
				| Some attribs ->
					use (loop attribs)
			with Not_found ->
				default
		in
		let kind = match jf.jf_kind with
			| JKField ->
				FVar(Some (convert_signature ctx p jf.jf_signature,p),None)
			| JKMethod ->
				begin match jf.jf_signature with
				| TMethod(args,ret) ->
					let local_names = extract_local_names() in
					let convert_arg i jsig =
						let name = local_names (i + 1) in
						((name,p),false,[],Some (convert_signature ctx p jsig,p),None)
					in
					let f = {
						f_params = List.map (fun tp -> convert_type_parameter ctx tp p) jf.jf_types;
						f_args = List.mapi convert_arg args;
						f_type = Some (Option.map_default (fun jsig -> convert_signature ctx p jsig,p) (ct_void,p) ret);
						f_expr = None;
					} in
					FFun f
				| _ ->
					assert false
				end
		in
		let cff = {
			cff_name = (name,p);
			cff_doc = None;
			cff_pos = p;
			cff_meta = !meta;
			cff_access = !access;
			cff_kind = kind;
		} in
		cff

	let convert_class ctx (jc : jclass) (file : string) =
		let p = {
			pfile = file;
			pmin = 0;
			pmax = 0
		} in
		let flags = ref [HExtern] in
		let meta = ref [] in
		let add_flag f = flags := f :: !flags in
		let add_meta m = meta := m :: !meta in
		add_meta (Meta.LibType,[],p);
		let is_interface = List.mem JInterface jc.cflags in
		if is_interface then add_flag HInterface;
		begin match jc.csuper with
			| TObject( (["java";"lang"], "Object"), _ ) ->
				()
			| jsig ->
				add_flag (HExtends (get_type_path (convert_signature ctx p jsig),p))
		end;
		List.iter (fun jsig ->
			let path = (get_type_path (convert_signature ctx p jsig),p) in
			if is_interface then
				add_flag (HExtends path)
			else
				add_flag (HImplements path)
		) jc.cinterfaces;
		let fields = DynArray.create () in
		let known_names = Hashtbl.create 0 in
		let known_sigs = Hashtbl.create 0 in
		let should_generate jf =
			try
				List.iter (function
					| JPrivate -> raise Exit
					| _ -> ()
				) jf.jf_flags;
				true
			with Exit ->
				false
		in
		if jc.cpath <> (["java";"lang"], "CharSequence") then begin
			List.iter (fun jf ->
				if should_generate jf then begin
					Hashtbl.replace known_names jf.jf_name jf;
					let sig_key = match jf.jf_signature with
						| TMethod(jsigs,_) -> TMethod(jsigs,None) (* lack of return type variance *)
						| jsig -> jsig
					in
					let key = (jf.jf_name,sig_key) in
					if not (Hashtbl.mem known_sigs key) then begin
						Hashtbl.add known_sigs key jf;
						DynArray.add fields (convert_field ctx jc jf p)
					end
				end
			) jc.cmethods;
			List.iter (fun jf ->
				if should_generate jf then begin
					if not (Hashtbl.mem known_names jf.jf_name) then begin
						Hashtbl.add known_names jf.jf_name jf;
						DynArray.add fields (convert_field ctx jc jf p)
					end
				end
			) jc.cfields;
		end;
		let _,class_name = jname_to_hx (snd jc.cpath) in
		add_meta (Meta.Native, [EConst (String (s_type_path jc.cpath,SDoubleQuotes) ),p],p);
		let d = {
			d_name = (class_name,p);
			d_doc = None;
			d_params = List.map (fun tp -> convert_type_parameter ctx tp p) jc.ctypes;
			d_meta = !meta;
			d_flags = !flags;
			d_data = DynArray.to_list fields;
		} in
		(EClass d,p)

	let convert_type ctx jc file =
		if List.mem JEnum jc.cflags then convert_enum jc file else convert_class ctx jc file

	let convert_module pack jcs =
		let types = List.map (fun (jc,_,file) ->
			let ctx = {
				type_params = type_param_lut PMap.empty jc.ctypes;
			} in
			convert_type ctx jc file;
		) jcs in
		(pack,types)
end

class java_library_modern com name file_path = object(self)
	inherit [java_lib_type,unit] native_library name file_path as super

	val zip = lazy (Zip.open_in file_path)
	val mutable cached_files = []
	val modules = Hashtbl.create 0
	val mutable loaded = false
	val mutable closed = false

	method load =
		if not loaded then begin
			loaded <- true;
			List.iter (function
				| ({ Zip.is_directory = false; Zip.filename = filename } as entry) when String.ends_with filename ".class" ->
					let pack = String.nsplit filename "/" in
					begin match List.rev pack with
						| [] -> ()
						| name :: pack ->
							let name = String.sub name 0 (String.length name - 6) in
							let pack = List.rev pack in
							let pack,(mname,tname) = jpath_to_hx (pack,name) in
							let path = jpath_to_path (pack,(mname,tname)) in
							let mname = match mname with
								| None -> tname
								| Some mname -> mname
							in
							Hashtbl.add modules (pack,mname) (filename,entry);
							cached_files <- path :: cached_files;
						end
				| _ -> ()
			) (Zip.entries (Lazy.force zip))
		end

	method private read zip (filename,entry) =
		let data = Zip.read_entry zip entry in
		(JReader.parse_class (IO.input_string data),file_path,file_path ^ "@" ^ filename)

	method lookup path : java_lib_type =
		None

	method close =
		if not closed then begin
			closed <- true;
			Zip.close_in (Lazy.force zip)
		end

	method list_modules : path list =
		cached_files

	method build path (p : pos) : Ast.package option =
		let build path =
			if path = (["java";"lang"],"String") then
				None
			else begin
				try
					let entries = Hashtbl.find_all modules path in
					if entries = [] then raise Not_found;
					let zip = Lazy.force zip in
					let jcs = List.map (self#read zip) entries in
					Some (Converter.convert_module (fst path) jcs)
				with Not_found ->
					None
			end
		in
		build path

	method get_data = ()
end