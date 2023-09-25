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
open Extlib_leftovers
open Swf
open As3
open As3hl
open Common
open Globals
open Ast
open NativeLibraries

let lowercase_pack pack =
	let rec loop acc pack =
		match pack with
		| [] -> List.rev acc
		| name :: rest ->
			let name =
				let fchar = String.get name 0 in
				if fchar >= 'A' && fchar <= 'Z' then
					(String.make 1 (Char.lowercase_ascii fchar)) ^ String.sub name 1 (String.length name - 1)
				else
					name
			in
			loop (name :: acc) rest
	in
	loop [] pack


let tp_dyn = { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None; }

let ct_dyn = CTPath tp_dyn

let ct_rest = CTPath {
	tpackage = ["haxe"];
	tname = "Rest";
	tparams = [TPType (ct_dyn,null_pos)];
	tsub = None;
}

let rec make_tpath = function
	| HMPath (pack,name) ->
		let pdyn = ref false in
		let pack, name = match pack, name with
			| [], "void" -> [], "Void"
			| [], "int" -> [], "Int"
			| [], "uint" -> [], "UInt"
			| [], "Number" -> [], "Float"
			| [], "Boolean" -> [], "Bool"
			| [], "Object" -> ["flash";"utils"], "Object"
			| [], "Function" -> ["flash";"utils"], "Function"
			| [], "Class" | [],"Array" -> pdyn := true; pack, name
			| [], "Error" -> ["flash";"errors"], "Error"
			| [] , "XML" -> ["flash";"xml"], "XML"
			| [] , "XMLList" -> ["flash";"xml"], "XMLList"
			| [] , "QName" -> ["flash";"utils"], "QName"
			| [] , "Namespace" -> ["flash";"utils"], "Namespace"
			| [] , "RegExp" -> ["flash";"utils"], "RegExp"
			| ["__AS3__";"vec"] , "Vector" -> pdyn := true; ["flash"], "Vector"
			| _ -> lowercase_pack pack, name
		in
		{
			tpackage = pack;
			tname = name;
			tparams = if !pdyn then [TPType (ct_dyn,null_pos)] else[];
			tsub = None;
		}
	| HMName (id,ns) ->
		{
			tpackage = (match ns with
				| HNInternal (Some ns) -> ExtString.String.nsplit ns "."
				| HNPrivate (Some ns) ->
					(try
						let file, line = ExtString.String.split ns ".as$" in
						[file ^ "_" ^ line]
					with _ ->
						[])
				| _ -> []);
			tname = id;
			tparams = [];
			tsub = None;
		}
	| HMNSAny (id) ->
		{
			tpackage = [];
			tname = id;
			tparams = [];
			tsub = None;
		}
	| HMMultiName _ ->
		die "" __LOC__
	| HMRuntimeName _ ->
		die "" __LOC__
	| HMRuntimeNameLate ->
		die "" __LOC__
	| HMMultiNameLate _ ->
		die "" __LOC__
	| HMAttrib _ ->
		die "" __LOC__
	| HMAny ->
		die "" __LOC__
	| HMParams (t,params) ->
		let params = List.map (fun t -> TPType (CTPath (make_tpath t),null_pos)) params in
		{ (make_tpath t) with tparams = params }

let make_topt = function
	| None -> tp_dyn
	| Some t -> make_tpath t

let make_type t = CTPath (make_topt t)

let make_dyn_type t =
	match make_topt t with
	| { tpackage = ["flash";"utils"]; tname = ("Object"|"Function") } -> make_type None
	| o -> CTPath o

let is_valid_path com pack name =
	let rec loop = function
		| [] ->
			false
		| (file,load) :: l ->
			match load (pack,name) null_pos with
			| None -> loop l
			| Some (_,a) -> true
	in
	let file = Printf.sprintf "%s/%s.hx" (String.concat "/" pack) name in
	loop com.load_extern_type || (try ignore(Common.find_file com file); true with Not_found -> false)

let build_class com c file =
	let path = make_tpath c.hlc_name in
	let pos = { pfile = file ^ "@" ^ s_type_path (path.tpackage,path.tname); pmin = 0; pmax = 0 } in
	match path with
	| { tpackage = ["flash";"utils"]; tname = ("Object"|"Function") } ->
		let inf = {
			d_name = path.tname,null_pos;
			d_doc = None;
			d_params = [];
			d_meta = [];
			d_flags = [];
			d_data = ct_dyn,null_pos;
		} in
		(path.tpackage, [(ETypedef inf,pos)])
	| _ ->
	(* make flags *)
	let flags = [HExtern] in
	let flags = if c.hlc_interface then HInterface :: flags else flags in
	let flags = (match c.hlc_super with
		| None | Some (HMPath ([],"Object")) -> flags
		| Some (HMPath ([],"Function")) -> flags (* found in AIR SDK *)
		| Some s -> HExtends (make_tpath s,null_pos) :: flags
	) in
	let flags = List.map (fun i ->
		let i = (match i with
			| HMMultiName (Some id,ns) ->
				let rec loop = function
					| [] -> HMPath ([],id)
					| HNPublic (Some ns) :: _ when is_valid_path com (ExtString.String.nsplit ns ".") id -> HMPath (ExtString.String.nsplit ns ".",id)
					| _ :: l -> loop l
				in
				loop ns
			| HMPath _ -> i
			| _ -> die "" __LOC__
		) in
		if c.hlc_interface then HExtends (make_tpath i,null_pos) else HImplements (make_tpath i,null_pos)
	) (Array.to_list c.hlc_implements) @ flags in
	let flags = if c.hlc_sealed || Common.defined com Define.FlashStrict then flags else HImplements (tp_dyn,null_pos) :: flags in
	(* make fields *)
	let getters = Hashtbl.create 0 in
	let setters = Hashtbl.create 0 in
	let override = Hashtbl.create 0 in
	let is_xml = (match path.tpackage, path.tname with
		| ["flash";"xml"], ("XML" | "XMLList") -> true
		| _ -> false
	) in
	let make_field stat acc f =
		let meta = ref [] in
		let flags = (match f.hlf_name with
			| HMPath _ -> [APublic,null_pos]
			| HMName (_,ns) ->
				(match ns with
				| HNPrivate _ | HNNamespace "http://www.adobe.com/2006/flex/mx/internal" -> []
				| HNNamespace ns ->
					if not (c.hlc_interface || is_xml) then meta := (Meta.Ns,[String(ns,SDoubleQuotes)]) :: !meta;
					[APublic,null_pos]
				| HNInternal (Some ns) ->
					if not (c.hlc_interface || is_xml) then meta := (Meta.Ns,[String(ns,SDoubleQuotes); Ident "internal"]) :: !meta;
					[APublic,null_pos]
				| HNExplicit _ | HNInternal _ | HNPublic _ ->
					[APublic,null_pos]
				| HNStaticProtected _ | HNProtected _ ->
					meta := (Meta.Protected,[]) :: !meta;
					[APrivate,null_pos])
			| _ -> []
		) in
		if flags = [] then acc else
		let flags = if stat then (AStatic,null_pos) :: flags else flags in
		let name = (make_tpath f.hlf_name).tname in
		let mk_meta() =
			List.map (fun (s,cl) -> s, List.map (fun c -> EConst c,pos) cl, pos) (!meta)
		in
		let cf = {
			cff_name = name,null_pos;
			cff_doc = None;
			cff_pos = pos;
			cff_meta = [];
			cff_access = flags;
			cff_kind = FVar (None,None);
		} in
		match f.hlf_kind with
		| HFVar v ->
			cf.cff_meta <- mk_meta();
			cf.cff_kind <- FVar (Some (make_dyn_type v.hlv_type,null_pos),None);
			if v.hlv_const then begin
				cf.cff_access <- (AFinal,null_pos) :: cf.cff_access;
			end;
			cf :: acc
		| HFMethod m when m.hlm_override ->
			Hashtbl.add override (name,stat) ();
			acc
		| HFMethod m ->
			(match m.hlm_kind with
			| MK3Normal ->
				let t = m.hlm_type in
				let p = ref 0 and pn = ref 0 in
				let make_type = if stat || name = "new" then make_dyn_type else make_type in
				let args = List.map (fun at ->
					let aname = (match t.hlmt_pnames with
						| None -> incr pn; "p" ^ string_of_int !pn
						| Some l ->
							match List.nth l !p with
							| None -> incr pn; "p" ^ string_of_int !pn
							| Some i -> i
					) in
					let opt_val = (match t.hlmt_dparams with
						| None -> None
						| Some l ->
							try
								Some (List.nth l (!p - List.length t.hlmt_args + List.length l))
							with
								_ -> None
					) in
					incr p;
					let t = make_type at in
					let is_opt = ref false in
					let def_val = match opt_val with
						| None -> None
						| Some v ->
							let v = (match v with
							| HVNone | HVNull | HVNamespace _ ->
								is_opt := true;
								None
							| HVString s ->
								is_opt := true;
								Some (String (s,SDoubleQuotes))
							| HVBool b ->
								Some (Ident (if b then "true" else "false"))
							| HVInt i | HVUInt i ->
								Some (Int (Int32.to_string i, None))
							| HVFloat f ->
								Some (Float (Numeric.float_repres f, None))
							) in
							match v with
							| None -> None
							| Some v ->
								(* add for -D gen-hx-classes generation *)
								meta := (Meta.DefParam,[String(aname,SDoubleQuotes);v]) :: !meta;
								Some (EConst v,pos)
					in
					((aname,null_pos),!is_opt,[],Some (t,null_pos),def_val)
				) t.hlmt_args in
				let args = if t.hlmt_var_args then
					args @ [("restArgs",null_pos),false,[],Some (ct_rest,null_pos),None]
				else args in
				let f = {
					f_params = [];
					f_args = args;
					f_type = Some (make_type t.hlmt_ret,null_pos);
					f_expr = None;
				} in
				cf.cff_meta <- mk_meta();
				cf.cff_kind <- FFun f;
				cf :: acc
			| MK3Getter ->
				Hashtbl.add getters (name,stat) (m.hlm_type.hlmt_ret,mk_meta());
				acc
			| MK3Setter ->
				Hashtbl.add setters (name,stat) ((match m.hlm_type.hlmt_args with [t] -> t | _ -> die "" __LOC__),mk_meta());
				acc
			)
		| _ -> acc
	in
	let fields = if c.hlc_interface then [] else make_field false [] {
		hlf_name = HMPath ([],"new");
		hlf_slot = 0;
		hlf_metas = None;
		hlf_kind = HFMethod {
			hlm_type = { c.hlc_construct with hlmt_ret = Some (HMPath ([],"void")) };
			hlm_final = false;
			hlm_override = false;
			hlm_kind = MK3Normal
		}
	} in
	let fields = Array.fold_left (make_field false) fields c.hlc_fields in
	let fields = Array.fold_left (make_field true) fields c.hlc_static_fields in
	let make_get_set name stat tget tset =
		let get, set, t, meta = (match tget, tset with
			| None, None -> die "" __LOC__
			| Some (t,meta), None -> true, false, t, meta
			| None, Some (t,meta) -> false, true, t, meta
			| Some (t1,meta1), Some (t2,meta2) -> true, true, (if t1 <> t2 then None else t1), meta1 @ (List.filter (fun m -> not (List.mem m meta1)) meta2)
		) in
		let t = if name = "endian" then Some (HMPath (["flash";"utils"],"Endian")) else t in
		let flags, accessor_flags = [APublic,null_pos], [APrivate,null_pos] in
		let flags, accessor_flags = if stat then (AStatic,null_pos) :: flags, (AStatic,null_pos) :: accessor_flags else flags, accessor_flags in
		let property_typehint = Some (make_dyn_type t,null_pos) in
		let fields = [] in
		let read_access, fields =
			if get then
				let getter = {
					cff_name = "get_" ^ name,null_pos;
					cff_pos = pos;
					cff_doc = None;
					cff_access = accessor_flags;
					cff_meta = [];
					cff_kind = FFun {
						f_params = [];
						f_args = [];
						f_type = property_typehint;
						f_expr = None;
					};
				} in
				("get",null_pos), getter :: fields
			else
				("never",null_pos), fields
		in
		let write_access, fields =
			if set then
				let setter = {
					cff_name = "set_" ^ name,null_pos;
					cff_pos = pos;
					cff_doc = None;
					cff_access = accessor_flags;
					cff_meta = [];
					cff_kind = FFun {
						f_params = [];
						f_args = [(("value",null_pos),false,[],property_typehint,None)];
						f_type = property_typehint;
						f_expr = None;
					};
				} in
				("set",null_pos), setter :: fields
			else
				("never",null_pos), fields
		in
		{
			cff_name = name,null_pos;
			cff_pos = pos;
			cff_doc = None;
			cff_access = flags;
			cff_meta = (Meta.FlashProperty,[],pos) :: meta;
			cff_kind = FProp (read_access,write_access,property_typehint,None);
		} :: fields
	in
	let fields = Hashtbl.fold (fun (name,stat) t acc ->
		if Hashtbl.mem override (name,stat) then acc else
		make_get_set name stat (Some t) (try Some (Hashtbl.find setters (name,stat)) with Not_found -> None) @ acc
	) getters fields in
	let fields = Hashtbl.fold (fun (name,stat) t acc ->
		if Hashtbl.mem getters (name,stat) || Hashtbl.mem override (name,stat) then
			acc
		else
			make_get_set name stat None (Some t) @ acc
	) setters fields in
	try
		(*
			If the class only contains static String constants, make it an enum
		*)
		let real_type = ref None in
		let rec loop = function
			| [] -> []
			| f :: l ->
				match f.cff_kind with
				| FVar (Some ((CTPath { tpackage = []; tname = ("String" | "Int" | "UInt")} as real_t),_),None)
				| FProp (("default",_),("never",_),Some ((CTPath { tpackage = []; tname = ("String" | "Int" | "UInt")}) as real_t,_),None) when List.mem_assoc AStatic f.cff_access ->
					(match !real_type with
					| None ->
						real_type := Some real_t
					| Some t ->
						if t <> real_t then raise Exit);
					{
						cff_name = f.cff_name;
						cff_doc = None;
						cff_pos = pos;
						cff_meta = [];
						cff_access = [];
						cff_kind = FVar (Some (real_t,null_pos), None);
					} :: loop l
				| FFun { f_args = [] } when fst f.cff_name = "new" -> loop l
				| _ -> raise Exit
		in
		List.iter (function HExtends _ | HImplements _ -> raise Exit | _ -> ()) flags;
		let constr = loop fields in
		let name = "enumAbstract:" ^ String.concat "." (path.tpackage @ [path.tname]) in
		if not (Common.raw_defined com name) then raise Exit;
		let native_path = s_type_path (path.tpackage, path.tname) in
		let real_type = Option.get !real_type in
		let abstract_data = {
			d_name = path.tname,null_pos;
			d_doc = None;
			d_params = [];
			d_meta = [(Meta.Native,[(EConst (String(native_path,SDoubleQuotes)),null_pos)],null_pos)];
			d_flags = [AbEnum;AbExtern; AbOver (real_type,pos); AbFrom (real_type,pos)];
			d_data = constr;
		} in
		(path.tpackage, [(EAbstract abstract_data,pos)])
	with Exit ->
	let flags = if c.hlc_final && List.exists (fun f -> fst f.cff_name <> "new" && not (List.mem_assoc AStatic f.cff_access)) fields then HFinal :: flags else flags in

	let meta =
		(* if the package was lowercased, add @:native("Original.Path") meta *)
		match c.hlc_name with
		| HMPath (pack,name) when (pack <> [] && pack <> path.tpackage) ->
			let native_path = (String.concat "." pack) ^ "." ^ name in
			[(Meta.Native,[(EConst (String(native_path,SDoubleQuotes)), pos)],pos)]
		| _ ->
			[]
	in

	let class_data = {
		d_name = path.tname,null_pos;
		d_doc = None;
		d_params = [];
		d_meta = meta;
		d_flags = flags;
		d_data = fields;
	} in
	(path.tpackage, [(EClass class_data,pos)])

let extract_data (_,tags) =
	let t = Timer.timer ["read";"swf"] in
	let h = Hashtbl.create 0 in
	let loop_field f =
		match f.hlf_kind with
		| HFClass c ->
			let path = make_tpath f.hlf_name in
			(match path with
			| { tpackage = []; tname = "Float" | "Bool" | "Int" | "UInt" | "Dynamic" } -> ()
			| { tpackage = _; tname = "MethodClosure" } -> ()
			| _ -> Hashtbl.add h (path.tpackage,path.tname) c)
		| _ -> ()
	in
	List.iter (fun t ->
		match t.tdata with
		| TActionScript3 (_,as3) ->
			List.iter (fun i -> Array.iter loop_field i.hls_fields) (As3hlparse.parse as3)
		| _ -> ()
	) tags;
	t();
	h

let remove_debug_infos as3 =
	let hl = As3hlparse.parse as3 in
	let methods = Hashtbl.create 0 in
	let rec loop_field f =
		{ f with hlf_kind = (match f.hlf_kind with
			| HFMethod m -> HFMethod { m with hlm_type = loop_method m.hlm_type }
			| HFFunction f -> HFFunction (loop_method f)
			| HFVar v -> HFVar v
			| HFClass c -> HFClass (loop_class c))
		}
	and loop_class c =
		(* mutate in order to preserve sharing *)
		c.hlc_construct <- loop_method c.hlc_construct;
		c.hlc_fields <- Array.map loop_field c.hlc_fields;
		c.hlc_static_construct <- loop_method c.hlc_static_construct;
		c.hlc_static_fields <- Array.map loop_field c.hlc_static_fields;
		c
	and loop_static s =
		{
			hls_method = loop_method s.hls_method;
			hls_fields = Array.map loop_field s.hls_fields;
		}
	and loop_method m =
		try
			Hashtbl.find methods m.hlmt_index
		with Not_found ->
			let m2 = { m with hlmt_debug_name = None; hlmt_pnames = None } in
			Hashtbl.add methods m.hlmt_index m2;
			m2.hlmt_function <- (match m.hlmt_function with None -> None | Some f -> Some (loop_function f));
			m2
	and loop_function f =
		let cur = ref 0 in
		let positions = MultiArray.map (fun op ->
			let p = !cur in
			(match op with
			| HDebugReg _ | HDebugLine _ | HDebugFile _ | HBreakPointLine _ | HTimestamp -> ()
			| _ -> incr cur);
			p
		) f.hlf_code in
		MultiArray.add positions (!cur);
		let code = MultiArray.create() in
		MultiArray.iteri (fun pos op ->
			match op with
			| HDebugReg _ | HDebugLine _ | HDebugFile _ | HBreakPointLine _ | HTimestamp -> ()
			| _ ->
				let p delta =
					MultiArray.get positions (pos + delta) - MultiArray.length code
				in
				let op = (match op with
				| HJump (j,delta) -> HJump (j, p delta)
				| HSwitch (d,deltas) -> HSwitch (p d,List.map p deltas)
				| HFunction m -> HFunction (loop_method m)
				| HCallStatic (m,args) -> HCallStatic (loop_method m,args)
				| HClassDef c -> HClassDef c (* mutated *)
				| _ -> op) in
				MultiArray.add code op
		) f.hlf_code;
		f.hlf_code <- code;
		f.hlf_trys <- Array.map (fun t ->
			{
				t with
				hltc_start = MultiArray.get positions t.hltc_start;
				hltc_end = MultiArray.get positions t.hltc_end;
				hltc_handle = MultiArray.get positions t.hltc_handle;
			}
		) f.hlf_trys;
		f
	in
	As3hlparse.flatten (List.map loop_static hl)

let parse_swf com file =
	let t = Timer.timer ["read";"swf"] in
	let is_swc = Path.file_extension file = "swc" || Path.file_extension file = "ane" in
	let ch = if is_swc then begin
		let zip = Zip.open_in file in
		try
			let entry = Zip.find_entry zip "library.swf" in
			let ch = IO.input_string (Zip.read_entry zip entry) in
			Zip.close_in zip;
			ch
		with _ ->
			Zip.close_in zip;
			failwith ("The input swc " ^ file ^ " is corrupted")
	end else
		IO.input_channel (open_in_bin file)
	in
	let h, tags = try
		Swf.parse ch
	with Out_of_memory ->
		failwith ("Out of memory while parsing " ^ file)
	| _ ->
		failwith ("The input swf " ^ file ^ " is corrupted")
	in
	IO.close_in ch;
	List.iter (fun t ->
		match t.tdata with
		| TActionScript3 (id,as3) ->
			t.tdata <- TActionScript3 (id,remove_debug_infos as3)
		| _ -> ()
	) tags;
	t();
	(h,tags)

class swf_library com name file_path = object(self)
	inherit [swf_lib_type,Swf.swf] native_library name file_path

	val mutable swf_data = None
	val mutable swf_classes = None
	val haxe_classes = Hashtbl.create 0

	method load =
		ignore(self#get_swf)

	method get_swf = match swf_data with
		| None ->
			let d = parse_swf com file_path in
			swf_data <- Some d;
			d
		| Some d ->
			d

	method extract = match swf_classes with
		| None ->
			let d = extract_data self#get_swf in
			swf_classes <- Some d;
			d
		| Some d ->
			d

	method lookup path =
		try Some (Hashtbl.find (self#extract) path)
		with Not_found -> None

	method list_modules =
		Hashtbl.fold (fun path _ acc -> path :: acc) (self#extract) []

	method close =
		()

	method build (path : path) (p : pos) : Ast.package option =
		try
			Some (Hashtbl.find haxe_classes path)
		with Not_found -> try
			let c = Hashtbl.find (self#extract) path in
			let c = build_class com c file_path in
			Hashtbl.add haxe_classes path c;
			Some c
		with Not_found ->
			None

	method get_data = self#get_swf
end

let add_swf_lib com file extern =
	let real_file = (try Common.find_file com file with Not_found -> failwith (" Library not found : " ^ file)) in
	let swf_lib = new swf_library com file real_file in
	if not extern then com.native_libs.swf_libs <- (swf_lib :> (swf_lib_type,Swf.swf) native_library) :: com.native_libs.swf_libs;
	CommonCache.handle_native_lib com swf_lib

let remove_classes toremove lib l =
	match !toremove with
	| [] -> lib
	| _ ->
		let hcl = Hashtbl.create 0 in
		List.iter (fun path -> Hashtbl.add hcl path ()) l;
		match List.filter (fun c -> Hashtbl.mem hcl c) (!toremove) with
		| [] -> lib
		| classes ->
			let rec tags = function
				| [] -> []
				| t :: l ->
					match t.tdata with
					| TActionScript3 (h,data) ->
						let data = As3hlparse.parse data in
						let loop f =
							match f.hlf_kind with
							| HFClass _ ->
								let path = make_tpath f.hlf_name in
								not (List.mem (path.tpackage,path.tname) classes)
							| _ -> true
						in
						let data = List.map (fun s -> { s with hls_fields = Array.of_list (List.filter loop (Array.to_list s.hls_fields)) }) data in
						let data = List.filter (fun s -> Array.length s.hls_fields > 0) data in
						(if data = [] then
							tags l
						else
							{ t with tdata = TActionScript3 (h,As3hlparse.flatten data) } :: tags l)
					| _ ->
						t :: tags l
			in
			toremove := List.filter (fun p -> not (List.mem p classes)) !toremove;
			fst lib, tags (snd lib)
