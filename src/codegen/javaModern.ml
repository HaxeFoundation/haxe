open Globals
open Ast
open ExtString
open NativeLibraries

module AccessFlags = struct
	type t =
		| MPublic
		| MPrivate
		| MProtected
		| MStatic
		| MFinal
		| MSynchronized
		| MBridge
		| MVarargs
		| MNative
		| MInterface
		| MAbstract
		| MStrict
		| MSynthetic
		| MEnum

	let to_int = function
		| MPublic -> 0x1
		| MPrivate -> 0x2
		| MProtected -> 0x4
		| MStatic -> 0x8
		| MFinal -> 0x10
		| MSynchronized -> 0x20
		| MBridge -> 0x40
		| MVarargs -> 0x80
		| MNative -> 0x100
		| MInterface -> 0x200
		| MAbstract -> 0x400
		| MStrict -> 0x800
		| MSynthetic -> 0x1000
		| MEnum -> 0x4000

	let has_flag b flag =
		b land (to_int flag) <> 0
end

module JDataHoldovers = struct
	type jwildcard =
		| WExtends (* + *)
		| WSuper (* -  *)
		| WNone

	type jtype_argument =
		| TType of jwildcard * jsignature
		| TAny (* * *)

	and jsignature =
		| TByte (* B *)
		| TChar (* C *)
		| TDouble (* D *)
		| TFloat (* F *)
		| TInt (* I *)
		| TLong (* J *)
		| TShort (* S *)
		| TBool (* Z *)
		| TObject of path * jtype_argument list (* L Classname *)
		| TObjectInner of (string list) * (string * jtype_argument list) list (* L Classname ClassTypeSignatureSuffix *)
		| TArray of jsignature * int option (* [ *)
		| TMethod of jmethod_signature (* ( *)
		| TTypeParameter of string (* T *)

	(* ( jsignature list ) ReturnDescriptor (| V | jsignature) *)
	and jmethod_signature = jsignature list * jsignature option

	type jtypes = (string * jsignature option * jsignature list) list

	type jannotation = {
		ann_type : jsignature;
		ann_elements : (string * jannotation_value) list;
	}

	and jannotation_value =
		| ValConst of jsignature * int
		| ValEnum of jsignature * string (* e *)
		| ValClass of jsignature (* c *) (* V -> Void *)
		| ValAnnotation of jannotation (* @ *)
		| ValArray of jannotation_value list (* [ *)
end

open JDataHoldovers

module JReaderHoldovers = struct
	open JDataHoldovers

	let rec parse_type_parameter_part s = match s.[0] with
		| '*' -> TAny, 1
		| c ->
			let wildcard, i = match c with
			| '+' -> WExtends, 1
			| '-' -> WSuper, 1
			| _ -> WNone, 0
			in
			let jsig, l = parse_signature_part (String.sub s i (String.length s - 1)) in
			(TType (wildcard, jsig), l + i)

	and parse_signature_part s =
		let len = String.length s in
		if len = 0 then raise Exit;
		match s.[0] with
		| 'B' -> TByte, 1
		| 'C' -> TChar, 1
		| 'D' -> TDouble, 1
		| 'F' -> TFloat, 1
		| 'I' -> TInt, 1
		| 'J' -> TLong, 1
		| 'S' -> TShort, 1
		| 'Z' -> TBool, 1
		| 'L' ->
			(try
			let orig_s = s in
			let rec loop start i acc =
				match s.[i] with
				| '/' -> loop (i + 1) (i + 1) (String.sub s start (i - start) :: acc)
				| ';' | '.' -> List.rev acc, (String.sub s start (i - start)), [], (i)
				| '<' ->
				let name = String.sub s start (i - start) in
				let rec loop_params i acc =
					let s = String.sub s i (len - i) in
					match s.[0] with
					| '>' -> List.rev acc, i + 1
					| _ ->
					let tp, l = parse_type_parameter_part s in
					loop_params (l + i) (tp :: acc)
				in
				let params, _end = loop_params (i + 1) [] in
				List.rev acc, name, params, (_end)
				| _ -> loop start (i+1) acc
			in
			let pack, name, params, _end = loop 1 1 [] in
			let rec loop_inner i acc =
				match s.[i] with
				| '.' ->
				let pack, name, params, _end = loop (i+1) (i+1) [] in
				if pack <> [] then failwith ("Inner types must not define packages. For '" ^ orig_s ^ "'.");
				loop_inner _end ( (name,params) :: acc )
				| ';' -> List.rev acc, i + 1
				| c -> failwith ("End of complex type signature expected after type parameter. Got '" ^ Char.escaped c ^ "' for '" ^ orig_s ^ "'." );
			in
			let inners, _end = loop_inner _end [] in
			match inners with
			| [] -> TObject((pack,name), params), _end
			| _ -> TObjectInner( pack, (name,params) :: inners ), _end
			with
			Invalid_string -> raise Exit)
		| '[' ->
			let p = ref 1 in
			while !p < String.length s && s.[!p] >= '0' && s.[!p] <= '9' do
			incr p;
			done;
			let size = (if !p > 1 then Some (int_of_string (String.sub s 1 (!p - 1))) else None) in
			let s , l = parse_signature_part (String.sub s !p (String.length s - !p)) in
			TArray (s,size) , l + !p
		| '(' ->
			let p = ref 1 in
			let args = ref [] in
			while !p < String.length s && s.[!p] <> ')' do
			let a , l = parse_signature_part (String.sub s !p (String.length s - !p)) in
			args := a :: !args;
			p := !p + l;
			done;
			incr p;
			if !p >= String.length s then raise Exit;
			let ret , l = (match s.[!p] with 'V' -> None , 1 | _ ->
			let s, l = parse_signature_part (String.sub s !p (String.length s - !p)) in
			Some s, l
			) in
			TMethod (List.rev !args,ret) , !p + l
		| 'T' ->
			(try
			let s1 , _ = String.split s ";" in
			let len = String.length s1 in
			TTypeParameter (String.sub s1 1 (len - 1)) , len + 1
			with
			Invalid_string -> raise Exit)
		| _ ->
			raise Exit

	let parse_signature s =
		try
			let sign , l = parse_signature_part s in
			if String.length s <> l then raise Exit;
			sign
		with
			Exit -> failwith ("Invalid signature '" ^ s ^ "'")

	let parse_method_signature s =
		match parse_signature s with
		| (TMethod m) -> m
		| _ -> failwith ("Unexpected signature '" ^ s ^ "'. Expecting method")

	let parse_formal_type_params s = match s.[0] with
		| '<' ->
			let rec read_id i =
			match s.[i] with
			| ':' | '>' -> i
			| _ -> read_id (i + 1)
			in
			let len = String.length s in
			let rec parse_params idx acc =
			let idi = read_id (idx + 1) in
			let id = String.sub s (idx + 1) (idi - idx - 1) in
			(* next must be a : *)
			(match s.[idi] with | ':' -> () | _ -> failwith ("Invalid formal type signature character: " ^ Char.escaped s.[idi] ^ " ; from " ^ s));
			let ext, l = match s.[idi + 1] with
				| ':' | '>' -> None, idi + 1
				| _ ->
				let sgn, l = parse_signature_part (String.sub s (idi + 1) (len - idi - 1)) in
				Some sgn, l + idi + 1
			in
			let rec loop idx acc =
				match s.[idx] with
				| ':' ->
				let ifacesig, ifacei = parse_signature_part (String.sub s (idx + 1) (len - idx - 1)) in
				loop (idx + ifacei + 1) (ifacesig :: acc)
				| _ -> acc, idx
			in
			let ifaces, idx = loop l [] in
			let acc = (id, ext, ifaces) :: acc in
			if s.[idx] = '>' then List.rev acc, idx + 1 else parse_params (idx - 1) acc
			in
			parse_params 0 []
		| _ -> [], 0

	let parse_throws s =
		let len = String.length s in
		let rec loop idx acc =
			if idx > len then raise Exit
			else if idx = len then acc, idx
			else match s.[idx] with
			| '^' ->
			let tsig, l = parse_signature_part (String.sub s (idx+1) (len - idx - 1)) in
			loop (idx + l + 1) (tsig :: acc)
			| _ -> acc, idx
		in
		loop 0 []

	let parse_complete_method_signature s =
		try
			let len = String.length s in
			let tparams, i = parse_formal_type_params s in
			let sign, l = parse_signature_part (String.sub s i (len - i)) in
			let throws, l2 = parse_throws (String.sub s (i+l) (len - i - l)) in
			if (i + l + l2) <> len then raise Exit;

			match sign with
			| TMethod msig -> tparams, msig, throws
			| _ -> raise Exit
		with
			Exit -> failwith ("Invalid method extended signature '" ^ s ^ "'")
end

module JReaderModern = struct
	open IO
	open IO.BigEndian

	open JReaderHoldovers

	type constant_pool = {
		strings : string array;
		paths : path array;
		name_and_types : (string * string) array;
	}

	type jlocal = {
		ld_start_pc : int;
		ld_length : int;
		ld_name : string;
		ld_descriptor : string;
		ld_index : int;
	}

	type jattribute =
		| AttrCode of jattribute list
		| AttrDeprecated
		| AttrLocalVariableTable of jlocal list
  		| AttrMethodParameters of (string * int) list
		| AttrSignature of string
		| AttrVisibleAnnotations of jannotation list
  		| AttrOther

	type jfield = {
		jf_name : string;
		jf_flags : int;
		jf_types : jtypes;
		jf_descriptor : jsignature;
		jf_attributes : jattribute list;
		jf_code : jattribute list option;
	}

	type jclass = {
		jc_path : path;
		jc_flags : int;
		jc_super : jsignature;
		jc_interfaces : jsignature list;
		jc_types : jtypes;
		jc_fields : jfield list;
		jc_methods : jfield list;
		jc_attributes : jattribute list;
	}

	let read_constant_pool ch =
		let count = read_ui16 ch in
		let strings = Array.make count "" in
		let paths = Array.make count 0 in
		let name_and_types = Array.make count (0,0) in
		let i = ref 1 in
		while !i < count do
			begin match read_byte ch with
			| 1 ->
				strings.(!i) <- nread_string ch (read_ui16 ch)
			| 3 ->
				ignore(read_real_i32 ch)
			| 4 ->
				ignore(read_float32 ch)
			| 5 ->
				incr i;
				ignore(read_i64 ch)
			| 6 ->
				incr i;
				ignore(read_double ch)
			| 7 ->
				paths.(!i) <- read_ui16 ch
			| 8 ->
				ignore(read_ui16 ch)
			| 9 | 10 | 11 ->
				ignore(read_ui16 ch);
				ignore(read_ui16 ch);
			| 12 ->
				let name = read_ui16 ch in
				let t = read_ui16 ch in
				name_and_types.(!i) <- (name,t);
			| 15 ->
				ignore(read_byte ch);
				ignore(read_ui16 ch);
			| 16 ->
				ignore(read_ui16 ch);
			| 17 | 18 ->
				ignore(read_ui16 ch);
				ignore(read_ui16 ch);
			| 19 | 20 ->
				ignore(read_ui16 ch);
			| i ->
				failwith (Printf.sprintf "Invalid constant pool byte: %i" i);
			end;
			incr i;
		done;
		let as_path s = match List.rev (String.nsplit s "/") with
			| [x] -> [],x
			| x :: l -> List.rev l,x
			| [] -> assert false
		in
		let paths = Array.map (fun index ->
			if index > 0 then as_path (strings.(index))
			else ([],"")
		) paths in
		let name_and_types = Array.map (fun (name,t) ->
			let name = if name > 0 then strings.(name) else "" in
			let t = if t > 0 then strings.(t) else "" in
			(name,t)
		) name_and_types in
		{strings;paths;name_and_types}


	let rec parse_element_value consts ch =
		let tag = IO.read_byte ch in
		match Char.chr tag with
		| 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's' ->
			let jsig = match (Char.chr tag) with
				| 's' ->
					TObject( (["java";"lang"],"String"), [] )
				| tag ->
					fst (parse_signature_part (Char.escaped tag))
			in
			ValConst(jsig,(read_ui16 ch))
		| 'e' ->
			let path = parse_signature (consts.strings.(read_ui16 ch)) in
			let name = consts.strings.(read_ui16 ch) in
			ValEnum (path, name)
		| 'c' ->
			let name = consts.strings.(read_ui16 ch) in
			let jsig = if name = "V" then
				TObject(([], "Void"), [])
			else
				parse_signature name
			in
			ValClass jsig
		| '@' ->
			ValAnnotation (parse_annotation consts ch)
		| '[' ->
			let num_vals = read_ui16 ch in
			ValArray (ExtList.List.init (num_vals) (fun _ -> parse_element_value consts ch))
		| tag ->
			failwith ("Invalid element value: '" ^  Char.escaped tag ^ "'")

	and parse_ann_element consts ch =
		let name = consts.strings.(read_ui16 ch) in
		let element_value = parse_element_value consts ch in
		name, element_value

	and parse_annotation consts ch =
		let anntype = parse_signature (consts.strings.(read_ui16 ch)) in
		let count = read_ui16 ch in
		{
			ann_type = anntype;
			ann_elements = ExtList.List.init count (fun _ -> parse_ann_element consts ch)
		}

	let rec parse_attribute consts ch =
		let name = consts.strings.(read_ui16 ch) in
		let length = read_i32 ch in
		match name with
		| "Code" ->
			ignore(read_ui16 ch); (* max stack *)
			ignore(read_ui16 ch); (* max locals *)
			let len = read_i32 ch in
			ignore(IO.nread_string ch len); (* code *)
			let len = read_ui16 ch in
			for i = 0 to len - 1 do
				ignore(IO.nread_string ch 8);
			done; (* exceptions *)
			let attribs = parse_attributes consts ch in
			AttrCode attribs
		| "Deprecated" ->
			AttrDeprecated
		| "LocalVariableTable" ->
			let len = read_ui16 ch in
			let locals = ExtList.List.init len (fun _ ->
				let start_pc = read_ui16 ch in
				let length = read_ui16 ch in
				let name = consts.strings.(read_ui16 ch) in
				let descriptor = consts.strings.(read_ui16 ch) in
				let index = read_ui16 ch in
				{
					ld_start_pc = start_pc;
					ld_length = length;
					ld_name = name;
					ld_descriptor = descriptor;
					ld_index = index
				}
			) in
			AttrLocalVariableTable locals
		| "MethodParameters" ->
			let len = IO.read_byte ch in
			let parameters = ExtList.List.init len (fun _ ->
				let name = consts.strings.(read_ui16 ch) in
				let flags = read_ui16 ch in
				(name,flags)
			) in
			AttrMethodParameters parameters
		| "RuntimeVisibleAnnotations" ->
    		let count = read_ui16 ch in
    		AttrVisibleAnnotations (ExtList.List.init count (fun _ -> parse_annotation consts ch))
		| "Signature" ->
			let s = consts.strings.(read_ui16 ch) in
			AttrSignature s
		| _ ->
			ignore(nread ch length);
			AttrOther

	and parse_attributes consts ch =
		Array.to_list (Array.init (read_ui16 ch) (fun _ ->
			parse_attribute consts ch
		))

	let parse_field consts is_method ch =
		let flags = read_ui16 ch in
		let name = consts.strings.(read_ui16 ch) in
		let descriptor = consts.strings.(read_ui16 ch) in
		let attributes = parse_attributes consts ch in
		let types = ref [] in
		let jsig = ref None in
		let code = ref None in
		List.iter (function
			| AttrCode code' ->
				code := Some code'
			| AttrSignature s ->
				if is_method then begin
					let tp, sgn, thr = parse_complete_method_signature s in
					types := tp;
					jsig := Some (TMethod(sgn));
				end else
					jsig := Some (parse_signature s)
			| _ ->
				()
		) attributes;
		{
			jf_name = name;
			jf_flags = flags;
			jf_types = !types;
			jf_descriptor = (match !jsig with
				| None -> parse_signature descriptor;
				| Some jsig -> jsig);
			jf_attributes = attributes;
			jf_code = !code;
		}

	let parse_class ch =
		if read_real_i32 ch <> 0xCAFEBABEl then failwith "Invalid header";
		let _ = read_ui16 ch in
		let _ = read_ui16 ch in
		let consts = read_constant_pool ch in
		let flags = read_ui16 ch in
		let this = consts.paths.(read_ui16 ch) in
		let super = TObject(consts.paths.(read_ui16 ch),[]) in
		let interfaces = ExtList.List.init (read_ui16 ch) (fun _ ->
			TObject(consts.paths.(read_ui16 ch),[])
		) in
		let fields = ExtList.List.init (read_ui16 ch) (fun _ -> parse_field consts false ch) in
		let methods = ExtList.List.init (read_ui16 ch) (fun _ -> parse_field consts true ch) in
		let attributes = parse_attributes consts ch in
		let types = ref [] in
		let interfaces = ref interfaces in
		let super = ref super in
		List.iter (function
			| AttrSignature s ->
				let formal, idx = parse_formal_type_params s in
				types := formal;
				let s = String.sub s idx (String.length s - idx) in
				let len = String.length s in
				let sup, idx = parse_signature_part s in
				let rec loop idx acc =
					if idx = len then
					acc
					else begin
					let s = String.sub s idx (len - idx) in
					let iface, i2 = parse_signature_part s in
					loop (idx + i2) (iface :: acc)
					end
				in
				interfaces := loop idx [];
				super := sup;
			| _ ->
				()
		) attributes;
		{
			jc_path = this;
			jc_flags = flags;
			jc_super = !super;
			jc_interfaces = !interfaces;
			jc_types = !types;
			jc_fields = fields;
			jc_methods = methods;
			jc_attributes = attributes;
		}
end

module PathConverter = struct
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
		| [x;""] ->
			None,x ^ "_" (* trailing $ *)
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
end

type java_lib_ctx = {
	type_params : (string,complex_type) PMap.t;
}

module SignatureConverter = struct
	open PathConverter

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
		| TMethod _ -> failwith "TMethod cannot be converted directly into Complex Type"
		| TTypeParameter s ->
			try
				PMap.find s ctx.type_params
			with Not_found ->
				ct_dynamic
end

let get_type_path ct = match ct with | CTPath p -> p | _ -> die "" __LOC__

module Converter = struct

	open JReaderModern
	open PathConverter
	open SignatureConverter

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
		List.iter (fun (jf : jfield) ->
			match jf.jf_descriptor with
			| TObject( path, [] ) when path = jc.jc_path && AccessFlags.has_flag jf.jf_flags MStatic && AccessFlags.has_flag jf.jf_flags MFinal ->
				data := { ec_name = jf.jf_name,p; ec_doc = None; ec_meta = []; ec_args = []; ec_pos = p; ec_params = []; ec_type = None; } :: !data;
			| _ -> ()
		) jc.jc_fields;
		let _,class_name = jname_to_hx (snd jc.jc_path) in
		add_meta (Meta.Native, [EConst (String (s_type_path jc.jc_path,SDoubleQuotes) ),p],p);
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

	let convert_field ctx is_method (jc : jclass) (is_interface : bool) (jf : jfield) p =
		let ctx = {
			type_params = type_param_lut ctx.type_params jf.jf_types;
		} in
		let p = {p with pfile = p.pfile ^ "@" ^ jf.jf_name} in
		let is_static = AccessFlags.has_flag jf.jf_flags MStatic in
		let access = ref [] in
		let meta = ref [] in
		let add_access a = access := a :: !access in
		let add_meta m = meta := m :: !meta in
		if is_static then add_access (AStatic,p);
		List.iter (function
			| AttrDeprecated when jc.jc_path <> (["java";"util"],"Date") ->
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
		if is_method then add_access (AOverload,p);
		if AccessFlags.has_flag jf.jf_flags MFinal then add_access (AFinal,p);
		if not is_interface && AccessFlags.has_flag jf.jf_flags MAbstract then add_access (AAbstract,p);
		let extract_local_names () =
			let default i =
				"param" ^ string_of_int i
			in
			let rec loop attribs = match attribs with
				| AttrLocalVariableTable locals :: _ ->
					let shift = if is_static then 0 else -1 in
					List.map (fun loc ->
						loc.ld_index + shift,loc.ld_name
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
		let kind = if not is_method then
				FVar(Some (convert_signature ctx p jf.jf_descriptor,p),None)
			else
				begin match jf.jf_descriptor with
				| TMethod(args,ret) ->
					let local_names = extract_local_names() in
					let args_count = List.length args
					and is_varargs = AccessFlags.has_flag jf.jf_flags MVarargs in
					let convert_arg i jsig =
						let name = local_names (i + 1) in
						let hx_sig =
							match jsig with
							| TArray (jsig1,_) when is_varargs && i + 1 = args_count ->
								mk_type_path (["haxe";"extern"], "Rest") [TPType (convert_signature ctx p jsig1,p)]
							| _ ->
								convert_signature ctx p jsig
						in
						((name,p),false,[],Some (hx_sig,p),None)
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
		let is_interface = AccessFlags.has_flag jc.jc_flags MInterface in
		if is_interface then add_flag HInterface
		else if AccessFlags.has_flag jc.jc_flags MAbstract then add_flag HAbstract;
		begin match jc.jc_super with
			| TObject(([],""),_)
			| TObject((["java";"lang"],"Object"),_) ->
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
		) jc.jc_interfaces;
		let fields = DynArray.create () in
		let known_names = Hashtbl.create 0 in
		let known_sigs = Hashtbl.create 0 in
		let should_generate jf =
			not (AccessFlags.has_flag jf.jf_flags MPrivate)
		in
		if jc.jc_path <> (["java";"lang"], "CharSequence") then begin
			List.iter (fun jf ->
				if should_generate jf then begin
					Hashtbl.replace known_names jf.jf_name jf;
					let sig_key = match jf.jf_descriptor with
						| TMethod(jsigs,_) -> TMethod(jsigs,None) (* lack of return type variance *)
						| jsig -> jsig
					in
					let key = (jf.jf_name,sig_key) in
					if not (Hashtbl.mem known_sigs key) then begin
						Hashtbl.add known_sigs key jf;
						DynArray.add fields (convert_field ctx true jc is_interface jf p)
					end
				end
			) jc.jc_methods;
			List.iter (fun jf ->
				if should_generate jf then begin
					if not (Hashtbl.mem known_names jf.jf_name) then begin
						Hashtbl.add known_names jf.jf_name jf;
						DynArray.add fields (convert_field ctx false jc is_interface jf p)
					end
				end
			) jc.jc_fields;
		end;
		let _,class_name = jname_to_hx (snd jc.jc_path) in
		add_meta (Meta.Native, [EConst (String (s_type_path jc.jc_path,SDoubleQuotes) ),p],p);
		let d = {
			d_name = (class_name,p);
			d_doc = None;
			d_params = List.map (fun tp -> convert_type_parameter ctx tp p) jc.jc_types;
			d_meta = !meta;
			d_flags = !flags;
			d_data = DynArray.to_list fields;
		} in
		(EClass d,p)

	let convert_type ctx jc file =
		if AccessFlags.has_flag jc.jc_flags MEnum then convert_enum jc file else convert_class ctx jc file

	let convert_module pack jcs =
		let types = List.map (fun (jc,_,file) ->
			let ctx = {
				type_params = type_param_lut PMap.empty jc.jc_types;
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
			let close = Timer.timer ["jar";"load"] in
			List.iter (function
				| ({ Zip.is_directory = false; Zip.filename = filename } as entry) when String.ends_with filename ".class" ->
					let pack = String.nsplit filename "/" in
					begin match List.rev pack with
						| [] -> ()
						| name :: pack ->
							let name = String.sub name 0 (String.length name - 6) in
							let pack = List.rev pack in
							let pack,(mname,tname) = PathConverter.jpath_to_hx (pack,name) in
							let path = PathConverter.jpath_to_path (pack,(mname,tname)) in
							let mname = match mname with
								| None ->
									cached_files <- path :: cached_files;
									tname
								| Some mname -> mname
							in
							Hashtbl.add modules (pack,mname) (filename,entry);
						end
				| _ -> ()
			) (Zip.entries (Lazy.force zip));
			close();
		end

	method private read zip (filename,entry) =
		Std.finally (Timer.timer ["jar";"read"]) (fun () ->
			let data = Zip.read_entry zip entry in
			let jc = JReaderModern.parse_class (IO.input_string data) in
			(jc,file_path,file_path ^ "@" ^ filename)
		) ()

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
					Std.finally (Timer.timer ["jar";"convert"]) (fun () ->
						Some (Converter.convert_module (fst path) jcs)
					) ();
				with Not_found ->
					None
			end
		in
		build path

	method get_data = ()
end