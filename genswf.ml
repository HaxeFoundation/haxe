(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Swf
open As3
open As3hl
open Genswf9
open Type
open Common
open Ast

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
			| ["__AS3__";"vec"] , "Vector" -> ["flash"], "Vector"
			| _ -> pack, name
		in
		{
			tpackage = pack;
			tname = name;
			tparams = if !pdyn then [TPType (CTPath { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None; })] else[];
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
	| HMMultiName _ ->
		assert false
	| HMRuntimeName _ ->
		assert false
	| HMRuntimeNameLate ->
		assert false
	| HMMultiNameLate _ ->
		assert false
	| HMAttrib _ ->
		assert false
	| HMAny ->
		assert false
	| HMParams (t,params) ->
		let params = List.map (fun t -> TPType (CTPath (make_tpath t))) params in
		{ (make_tpath t) with tparams = params }

let make_param cl p =
	{ tpackage = fst cl; tname = snd cl; tparams = [TPType (CTPath { tpackage = fst p; tname = snd p; tparams = []; tsub = None })]; tsub = None }

let make_topt = function
	| None -> { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None }
	| Some t -> make_tpath t

let make_type t = CTPath (make_topt t)

let make_dyn_type t =
	match make_topt t with
	| { tpackage = ["flash";"utils"]; tname = ("Object"|"Function") } -> make_type None
	| o -> CTPath o

let build_class com c file =
	let path = make_tpath c.hlc_name in
	let pos = { pfile = file ^ "@" ^ s_type_path (path.tpackage,path.tname); pmin = 0; pmax = 0 } in
	match path with
	| { tpackage = ["flash";"utils"]; tname = ("Object"|"Function") } ->
		let inf = {
			d_name = path.tname;
			d_doc = None;
			d_params = [];
			d_meta = [];
			d_flags = [];
			d_data = CTPath { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None; };
		} in
		(path.tpackage, [(ETypedef inf,pos)])
	| _ ->
  (* make flags *)
	let flags = [HExtern] in
	let flags = if c.hlc_interface then HInterface :: flags else flags in
	let flags = (match c.hlc_super with
		| None | Some (HMPath ([],"Object")) -> flags
		| Some s -> HExtends (make_tpath s) :: flags
	) in
	let flags = List.map (fun i ->
		let i = (match i with
			| HMMultiName (Some id,ns) ->
				let rec loop = function
					| [] -> HMPath ([],id)
					| HNPublic (Some ns) :: _ -> HMPath (ExtString.String.nsplit ns ".",id)
					| _ :: l -> loop l
				in
				loop (List.rev ns)
			| _ -> assert false
		) in
		HImplements (make_tpath i)
	) (Array.to_list c.hlc_implements) @ flags in
	let flags = if c.hlc_sealed || Common.defined com "flash_strict" then flags else HImplements (make_tpath (HMPath ([],"Dynamic"))) :: flags in
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
			| HMPath _ -> [APublic]
			| HMName (_,ns) ->
				(match ns with
				| HNPrivate _ | HNNamespace "http://www.adobe.com/2006/flex/mx/internal" -> []
				| HNNamespace ns ->
					if not (c.hlc_interface || is_xml) then meta := (":ns",[String ns]) :: !meta;
					[APublic]
				| HNExplicit _ | HNInternal _ | HNPublic _ ->
					[APublic]
				| HNStaticProtected _ | HNProtected _ ->
					meta := (":protected",[]) :: !meta;
					[APrivate])
			| _ -> []
		) in
		if flags = [] then acc else
		let flags = if stat then AStatic :: flags else flags in
		let name = (make_tpath f.hlf_name).tname in
		let mk_meta() =
			List.map (fun (s,cl) -> s, List.map (fun c -> EConst c,pos) cl, pos) (!meta)
		in
		let cf = {
			cff_name = name;
			cff_doc = None;
			cff_pos = pos;
			cff_meta = mk_meta();
			cff_access = flags;
			cff_kind = FVar (None,None);
		} in
		match f.hlf_kind with
		| HFVar v ->
			if v.hlv_const then
				cf.cff_kind <- FProp ("default","never",Some (make_type v.hlv_type),None)
			else
				cf.cff_kind <- FVar (Some (make_dyn_type v.hlv_type),None);
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
							| HVNone | HVNull | HVNamespace _ | HVString _ ->
								is_opt := true;
								None
							| HVBool b ->
								Some (Ident (if b then "true" else "false"))
							| HVInt i | HVUInt i ->
								Some (Int (Int32.to_string i))
							| HVFloat f ->
								Some (Float (string_of_float f))
							) in
							match v with
							| None -> None
							| Some v ->
								(* add for --gen-hx-classes generation *)
								meta := (":defparam",[String aname;v]) :: !meta;
								Some (EConst v,pos)
					in
					(aname,!is_opt,Some t,def_val)
				) t.hlmt_args in
				let args = if t.hlmt_var_args then
					args @ List.map (fun _ -> incr pn; ("p" ^ string_of_int !pn,true,Some (make_type None),None)) [1;2;3;4;5]
				else args in
				let f = {
					f_params = [];
					f_args = args;
					f_type = Some (make_type t.hlmt_ret);
					f_expr = None;
				} in
				cf.cff_meta <- mk_meta();
				cf.cff_kind <- FFun f;
				cf :: acc
			| MK3Getter ->
				Hashtbl.add getters (name,stat) m.hlm_type.hlmt_ret;
				acc
			| MK3Setter ->
				Hashtbl.add setters (name,stat) (match m.hlm_type.hlmt_args with [t] -> t | _ -> assert false);
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
		let get, set, t = (match tget, tset with
			| None, None -> assert false
			| Some t, None -> true, false, t
			| None, Some t -> false, true, t
			| Some t1, Some t2 -> true, true, (if t1 <> t2 then None else t1)
		) in
		let flags = [APublic] in
		let flags = if stat then AStatic :: flags else flags in
		{
			cff_name = name;
			cff_pos = pos;
			cff_doc = None;
			cff_access = flags;
			cff_meta = [];
			cff_kind = if get && set then FVar (Some (make_dyn_type t), None) else FProp ((if get then "default" else "never"),(if set then "default" else "never"),Some (make_dyn_type t),None);
		}
	in
	let fields = Hashtbl.fold (fun (name,stat) t acc ->
		if Hashtbl.mem override (name,stat) then acc else
		make_get_set name stat (Some t) (try Some (Hashtbl.find setters (name,stat)) with Not_found -> None) :: acc
	) getters fields in
	let fields = Hashtbl.fold (fun (name,stat) t acc ->
		if Hashtbl.mem getters (name,stat) || Hashtbl.mem override (name,stat) then
			acc
		else
			make_get_set name stat None (Some t) :: acc
	) setters fields in
	try
		(*
			If the class only contains static String constants, make it an enum
		*)
		let real_type = ref "" in
		let rec loop = function
			| [] -> []
			| f :: l ->
				match f.cff_kind with
				| FVar (Some (CTPath { tpackage = []; tname = ("String" | "Int" | "UInt") as tname }),None) when List.mem AStatic f.cff_access ->
					if !real_type = "" then real_type := tname else if !real_type <> tname then raise Exit;
					(f.cff_name,None,[],[],pos) :: loop l
				| FFun { f_args = [] } when f.cff_name = "new" -> loop l
				| _ -> raise Exit
		in
		List.iter (function HExtends _ | HImplements _ -> raise Exit | _ -> ()) flags;
		let constr = loop fields in
		let name = "fakeEnum:" ^ String.concat "." (path.tpackage @ [path.tname]) in
		if not (Common.defined com name) then raise Exit;
		let enum_data = {
			d_name = path.tname;
			d_doc = None;
			d_params = [];
			d_meta = [(":fakeEnum",[EConst (Ident !real_type),pos],pos)];
			d_flags = [EExtern];
			d_data = constr;
		} in
		(path.tpackage, [(EEnum enum_data,pos)])
	with Exit ->
	let class_data = {
		d_name = path.tname;
		d_doc = None;
		d_params = [];
		d_meta = if c.hlc_final && List.exists (fun f -> f.cff_name <> "new" && not (List.mem AStatic f.cff_access)) fields then [":final",[],pos] else [];
		d_flags = flags;
		d_data = fields;
	} in
	(path.tpackage, [(EClass class_data,pos)])

let extract_data (_,tags) =
	let t = Common.timer "read swf" in
	let h = Hashtbl.create 0 in
	let rec loop_field f =
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
		let positions = Array.map (fun op ->
			let p = !cur in
			(match op with
			| HDebugReg _ | HDebugLine _ | HDebugFile _ | HBreakPointLine _ | HTimestamp -> ()
			| _ -> incr cur);
			p
		) f.hlf_code in
		let positions = Array.concat [positions;[|!cur|]] in
		let code = DynArray.create() in
		Array.iteri (fun pos op ->
			match op with
			| HDebugReg _ | HDebugLine _ | HDebugFile _ | HBreakPointLine _ | HTimestamp -> ()
			| _ ->
				let p delta =
					positions.(pos + delta) - DynArray.length code
				in
				let op = (match op with
				| HJump (j,delta) -> HJump (j, p delta)
				| HSwitch (d,deltas) -> HSwitch (p d,List.map p deltas)
				| HFunction m -> HFunction (loop_method m)
				| HCallStatic (m,args) -> HCallStatic (loop_method m,args)
				| HClassDef c -> HClassDef c (* mutated *)
				| _ -> op) in
				DynArray.add code op
		) f.hlf_code;
		f.hlf_code <- DynArray.to_array code;
		f.hlf_trys <- Array.map (fun t ->
			{
				t with
				hltc_start = positions.(t.hltc_start);
				hltc_end = positions.(t.hltc_end);
				hltc_handle = positions.(t.hltc_handle);
			}
		) f.hlf_trys;
		f
	in
	As3hlparse.flatten (List.map loop_static hl)

let parse_swf com file =
	let t = Common.timer "read swf" in
	let file = (try Common.find_file com file with Not_found -> failwith ("SWF Library not found : " ^ file)) in
	let ch = IO.input_channel (open_in_bin file) in
	let h, tags = (try Swf.parse ch with _ -> failwith ("The input swf " ^ file ^ " is corrupted")) in
	IO.close_in ch;
	List.iter (fun t ->
		match t.tdata with
		| TActionScript3 (id,as3) when not com.debug && not com.display ->
			t.tdata <- TActionScript3 (id,remove_debug_infos as3)
		| _ -> ()
	) tags;
	t();
	(h,tags)

let add_swf_lib com file =
	let swf_data = ref None in
	let swf_classes = ref None in
	let getSWF = (fun() ->
		match !swf_data with
		| None ->
			let d = parse_swf com file in
			swf_data := Some d;
			d
		| Some d -> d
	) in
	let extract = (fun() ->
		match !swf_classes with
		| None ->
			let d = extract_data (getSWF()) in
			swf_classes := Some d;
			d
		| Some d -> d
	) in
	let build cl p =
		match (try Some (Hashtbl.find (extract()) cl) with Not_found -> None) with
		| None -> None
		| Some c -> Some (file, build_class com c file)
	in
	com.load_extern_type <- com.load_extern_type @ [build];
	com.swf_libs <- (file,getSWF,extract) :: com.swf_libs

(* ------------------------------- *)

let tag ?(ext=false) d = {
	tid = 0;
	textended = ext;
	tdata = d;
}

let swf_ver = function
	| 6. -> 6
	| 7. -> 7
	| 8. -> 8
	| 9. -> 9
	| 10. | 10.1 -> 10
	| 10.2 -> 11
	| 10.3 -> 12
	| 11. -> 13
	| 11.1 -> 14
	| 11.2 -> 15
	| 11.3 -> 16
	| 11.4 -> 17
	| _ -> assert false

let convert_header com (w,h,fps,bg) =
	let high = (max w h) * 20 in
	let rec loop b =		
		if 1 lsl b > high then b else loop (b + 1)
	in
	let bits = loop 0 in
	{
		h_version = swf_ver com.flash_version;
		h_size = {
			rect_nbits = bits + 1;
			left = 0;
			top = 0;
			right = w * 20;
			bottom = h * 20;
		};
		h_frame_count = 1;
		h_fps = to_float16 (if fps > 127.0 then 127.0 else fps);
		h_compressed = not (Common.defined com "no-swf-compress");
	} , bg

let default_header com =
	convert_header com (400,300,30.,0xFFFFFF)

type dependency_kind =
	| DKInherit
	| DKExpr
	| DKType

let build_dependencies t =
	let h = ref PMap.empty in
	let add_path p k =
		h := PMap.add (p,k) () !h;
	in
	let rec add_type_rec l t =
		if List.memq t l then () else
		match t with
		| TEnum (e,pl) ->
			add_path e.e_path DKType;
			List.iter (add_type_rec (t::l)) pl;
		| TInst (c,pl) ->
			add_path c.cl_path DKType;
			List.iter (add_type_rec (t::l)) pl;
		| TAbstract (a,pl) ->
			add_path a.a_path DKType;
			List.iter (add_type_rec (t::l)) pl;
		| TFun (pl,t2) ->
			List.iter (fun (_,_,t2) -> add_type_rec (t::l) t2) pl;
			add_type_rec (t::l) t2;
		| TAnon a ->
			PMap.iter (fun _ f -> add_type_rec (t::l) f.cf_type) a.a_fields
		| TDynamic t2 ->
			add_type_rec (t::l) t2;
		| TLazy f ->
			add_type_rec l ((!f)())
		| TMono r ->
			(match !r with
			| None -> ()
			| Some t -> add_type_rec l t)
		| TType (tt,pl) ->
			add_type_rec (t::l) tt.t_type;
			List.iter (add_type_rec (t::l)) pl
	and add_type t =
		add_type_rec [] t
	and add_expr e =
		match e.eexpr with
		| TTypeExpr t -> add_path (Type.t_path t) DKExpr
		| TEnumField (e,_) -> add_path e.e_path DKExpr
		| TNew (c,pl,el) ->
			add_path c.cl_path DKExpr;
			List.iter add_type pl;
			List.iter add_expr el;
		| TFunction f ->
			List.iter (fun (v,_) -> add_type v.v_type) f.tf_args;
			add_type f.tf_type;
			add_expr f.tf_expr;
		| TFor (v,e1,e2) ->
			add_type v.v_type;
			add_expr e1;
			add_expr e2;
		| TVars vl ->
			List.iter (fun (v,e) ->
				add_type v.v_type;
				match e with
				| None -> ()
				| Some e -> add_expr e
			) vl
		| _ ->
			Type.iter add_expr e
	and add_field f =
		add_type f.cf_type;
		match f.cf_expr with
		| None -> ()
		| Some e -> add_expr e
	in
	let add_inherit (c,pl) =
		add_path c.cl_path DKInherit;
		List.iter add_type pl;
	in
	(match t with
	| TClassDecl c when not c.cl_extern ->
		List.iter add_field c.cl_ordered_fields;
		List.iter add_field c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f ->
			add_field f;
			if c.cl_path <> (["flash"],"Boot") then add_path (["flash"],"Boot") DKExpr;
		);
		(match c.cl_init with
		| None -> ()
		| Some e -> add_expr e);
		(match c.cl_super with
		| None -> add_path ([],"Object") DKInherit;
		| Some x -> add_inherit x);
		List.iter (fun (_,t) ->
			(* add type-parameters constraints dependencies *)
			match follow t with
			| TInst (c,_) -> List.iter add_inherit c.cl_implements
			| _ -> ()
		) c.cl_types;
		List.iter add_inherit c.cl_implements;
	| TEnumDecl e when not e.e_extern ->
		PMap.iter (fun _ f -> add_type f.ef_type) e.e_constrs;
	| _ -> ());
	h := PMap.remove (([],"Int"),DKType) (!h);
	h := PMap.remove (([],"Int"),DKExpr) (!h);
	PMap.foldi (fun (c,k) () acc -> (c,k) :: acc) (!h) []

let build_swc_catalog com types =
	let node x att l =
		Xml.Element (x,att,l)
	in
	let make_path t sep =
		let path, name = t_path t in
		String.concat sep (path @ [name])
	in
	let make_id path =
		match Genswf9.real_path path with
		| [],n -> n
		| l,n -> (String.concat "." l) ^ ":" ^ n
	in
	let build_script t =
		let deps = build_dependencies t in
		node "script" [("name",make_path t "/");("mod","0")] ([
			node "def" ["id",make_id (t_path t)] [];
			node "dep" [("id","AS3");("type","n")] [];
		] @ List.map (fun (p,k) ->
			let t = (match k with
				| DKInherit -> "i"
				| DKExpr -> (match p with "flash" :: _ :: _ , _ -> "i" | _ -> "e")
				| DKType -> "s"
			) in
			node "dep" [("id",make_id p);("type",t)] []
		) deps)
	in
	let x = node "swc" ["xmlns","http://www.adobe.com/flash/swccatalog/9"] [
		node "versions" [] [
			node "swc" ["version","1.2"] [];
			node "haxe" ["version",Printf.sprintf "%d.%.2d" (com.version/100) (com.version mod 100)] [];
		];
		node "features" [] [
			node "feature-script-deps" [] [];
			node "feature-files" [] [];
		];
		node "libraries" [] [
			node "library" ["path","library.swf"] (List.map build_script types)
		];
		node "files" [] [];
	] in
	"<?xml version=\"1.0\" encoding =\"utf-8\"?>\n" ^ Xml.to_string_fmt x

let remove_classes toremove lib hcl =
	let lib = lib() in
	match !toremove with
	| [] -> lib
	| _ ->
		let hcl = hcl() in
		match List.filter (fun c -> Hashtbl.mem hcl c) (!toremove) with
		| [] -> lib
		| classes ->
			let rec tags = function
				| [] -> []
				| t :: l ->
					match t.tdata with
					| TActionScript3 (h,data) ->
						let data = As3hlparse.parse data in
						let rec loop f =
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

let build_swf8 com codeclip exports =
	let code, clips = Genswf8.generate com in
	let cid = ref 0 in
	let clips = List.fold_left (fun acc m ->
		let ename = Ast.s_type_path m in
		if Hashtbl.mem exports ename then
			acc
		else begin
			incr cid;
			tag ~ext:true (TClip { c_id = !cid; c_frame_count = 1; c_tags = [] }) ::
			tag ~ext:true (TExport [{ exp_id = !cid; exp_name = ename }]) ::
			acc
		end;
	) [] clips in
	let code = (match codeclip with
		| None -> List.map tag code
		| Some link ->
			incr cid;
			[
				tag (TClip {
					c_id = !cid;
					c_frame_count = 1;
					c_tags = List.map tag code @ [tag TShowFrame];
				});
				tag (TExport [{ exp_id = !cid; exp_name = link }]);
			]
	) in
	clips @ code

type bitmap_format =
	| BJPG
	| BPNG

let detect_format file p =
	let ch = (try open_in_bin file with _ -> error "Could not open file" p) in
	let fmt = (match (try let a = input_byte ch in a, input_byte ch with _ -> 0,0) with
		| 0xFF, 0xD8 -> BJPG
		| 0x89, 0x50 -> BPNG
		| x,y -> 
			close_in ch;
			error "Unknown image file format" p
	) in
	close_in ch;
	fmt

let build_swf9 com file swc =
	let boot_name = if swc <> None || Common.defined com "haxe-boot" then "haxe" else "boot_" ^ (String.sub (Digest.to_hex (Digest.string (Filename.basename file))) 0 4) in
	let code = Genswf9.generate com boot_name in
	let code = (match swc with
	| Some cat ->
		cat := build_swc_catalog com (List.map (fun (t,_,_) -> t) code);
		List.map (fun (t,m,f) ->
			let path = (match t_path t with
				| [], name -> name
				| path, name -> String.concat "/" path ^ "/" ^ name
			) in
			let init = {
				hls_method = m;
				hls_fields = [|f|];
			} in
			tag (TActionScript3 (Some (1,path),As3hlparse.flatten [init]))
		) code
	| None ->
		let inits = List.map (fun (_,m,f) ->
			{
				hls_method = m;
				hls_fields = [|f|];
			}
		) code in
		[tag (TActionScript3 (None,As3hlparse.flatten inits))]
	) in
	let cid = ref 0 in
	let classes = ref [{ f9_cid = None; f9_classname = boot_name }] in
	let res = Hashtbl.fold (fun name data acc ->
		incr cid;
		classes := { f9_cid = Some !cid; f9_classname = s_type_path (Genswf9.resource_path name) } :: !classes;
		tag (TBinaryData (!cid,data)) :: acc
	) com.resources [] in
	let bmp = List.fold_left (fun acc t ->
		match t with
		| TClassDecl c ->
			let rec loop = function
				| [] -> acc
				| (":bitmap",[EConst (String file),p],_) :: l ->
					let file = try Common.find_file com file with Not_found -> file in
					let data = (try Std.input_file ~bin:true file with _  -> error "File not found" p) in
					incr cid;
					classes := { f9_cid = Some !cid; f9_classname = s_type_path c.cl_path } :: !classes;
					let raw() =
						tag (TBitsJPEG2 { bd_id = !cid; bd_data = data; bd_table = None; bd_alpha = None; bd_deblock = Some 0 })
					in
					let t = (match detect_format file p with
						| BPNG ->
							(*
								There is a bug in Flash PNG decoder for 24-bits PNGs : Color such has 0xFF00FF is decoded as 0xFE00FE.
								In that particular case, we will then embed the decoded PNG bytes instead.
							*)
							(try
								let png = Png.parse (IO.input_string data) in
								let h = Png.header png in
								(match h.Png.png_color with
								| Png.ClTrueColor (Png.TBits8,Png.NoAlpha) ->
									let data = Extc.unzip (Png.data png) in
									let raw_data = Png.filter png data in
									let cmp_data = Extc.zip raw_data in
									tag ~ext:true (TBitsLossless2 { bll_id = !cid; bll_format = 5; bll_width = h.Png.png_width; bll_height = h.Png.png_height; bll_data = cmp_data })
								| _ -> raw())
							with Exit ->
								raw())
						| _ -> raw()
					) in
					t :: loop l
				| (":bitmap",[EConst (String dfile),p1;EConst (String afile),p2],_) :: l ->
					let dfile = try Common.find_file com dfile with Not_found -> dfile in
					let afile = try Common.find_file com afile with Not_found -> afile in
					(match detect_format dfile p1 with
					| BJPG -> ()
					| _ -> error "RGB channel must be a JPG file" p1);
					(match detect_format afile p2 with
					| BPNG -> ()
					| _ -> error "Alpha channel must be a PNG file" p2);
					let ddata = Std.input_file ~bin:true dfile in
					let adata = Std.input_file ~bin:true afile in
					let png = Png.parse (IO.input_string adata) in
					let h = Png.header png in
					let amask = (match h.Png.png_color with
						| Png.ClTrueColor (Png.TBits8,Png.HaveAlpha) ->
							let data = Extc.unzip (Png.data png) in
							let raw_data = Png.filter png data in
							let alpha = String.make (h.Png.png_width * h.Png.png_height) '\000' in
							for i = 0 to String.length alpha do
								String.unsafe_set alpha i (String.unsafe_get raw_data (i lsl 2));
							done;
							Extc.zip alpha
						| _ -> error "PNG file must contain 8 bit alpha channel" p2
					) in
					incr cid;
					classes := { f9_cid = Some !cid; f9_classname = s_type_path c.cl_path } :: !classes;
					tag (TBitsJPEG3 { bd_id = !cid; bd_data = ddata; bd_table = None; bd_alpha = Some amask; bd_deblock = Some 0 }) :: loop l
				| (":file",[EConst (String file),p],_) :: l ->
					let file = try Common.find_file com file with Not_found -> file in
					let data = (try Std.input_file ~bin:true file with _  -> error "File not found" p) in
					incr cid;
					classes := { f9_cid = Some !cid; f9_classname = s_type_path c.cl_path } :: !classes;
					tag (TBinaryData (!cid,data)) :: loop l
				| (":sound",[EConst (String file),p],_) :: l ->
					let file = try Common.find_file com file with Not_found -> file in
					let data = (try Std.input_file ~bin:true file with _  -> error "File not found" p) in
					let make_flags fmt mono freq bits =
						let fbits = (match freq with 5512 when fmt <> 2 -> 0 | 11025 -> 1 | 22050 -> 2 | 44100 -> 3 | _ -> failwith ("Unsupported frequency " ^ string_of_int freq)) in
						let bbits = (match bits with 8 -> 0 | 16 -> 1 | _ -> failwith ("Unsupported bits " ^ string_of_int bits)) in
						(fmt lsl 4) lor (fbits lsl 2) lor (bbits lsl 1) lor (if mono then 0 else 1)
					in
					let flags, samples, data = (match file_extension file with
						| "wav" ->
							(try
								let i = IO.input_string data in
								if IO.nread i 4 <> "RIFF" then raise Exit;
								ignore(IO.nread i 4); (* size *)
								if IO.nread i 4 <> "WAVE" || IO.nread i 4 <> "fmt " || IO.read_i32 i <> 0x10 then raise Exit;
								if IO.read_ui16 i <> 1 then failwith "Not a PCM file";
								let chan = IO.read_ui16 i in
								if chan > 2 then failwith "Too many channels";
								let freq = IO.read_i32 i in
								ignore(IO.read_i32 i);
								ignore(IO.read_i16 i);
								let bits = IO.read_ui16 i in
								if IO.nread i 4 <> "data" then raise Exit;
								let data_size = IO.read_i32 i in
								let data = IO.nread i data_size in
								make_flags 0 (chan = 1) freq bits, (data_size * 8 / (chan * bits)), data
							with Exit | IO.No_more_input | IO.Overflow _ ->
								error "Invalid WAV file" p
							| Failure msg ->
								error ("Invalid WAV file (" ^ msg ^ ")") p
							)
						| "mp3" ->
							(try
								let i = IO.input_string data in
								if IO.read_byte i <> 0xFF then raise Exit;
								let ver = ((IO.read_byte i) lsr 3) land 3 in
								let sampling = [|11025;0;22050;44100|].(ver) in
								ignore(IO.read_byte i);
								let mono = (IO.read_byte i) lsr 6 = 3 in
								let samples = ref 0 in
								let i = IO.input_string data in
								let rec read_frame() =
									match (try IO.read_byte i with IO.No_more_input -> -1) with
									| -1 ->
										()
									| 73 ->
										(* ID3 *)
										if IO.nread i 2 <> "D3" then raise Exit;
										ignore(IO.read_ui16 i); (* version *)
										ignore(IO.read_byte i); (* flags *)
										let size = IO.read_byte i land 0x7F in
										let size = size lsl 7 lor (IO.read_byte i land 0x7F) in
										let size = size lsl 7 lor (IO.read_byte i land 0x7F) in
										let size = size lsl 7 lor (IO.read_byte i land 0x7F) in
										ignore(IO.nread i size); (* id3 data *)
										read_frame()
									| 0xFF ->
										let infos = IO.read_byte i in
										let ver = (infos lsr 3) land 3 in
										let layer = (infos lsr 1) land 3 in
										let bits = IO.read_byte i in
										let bitrate = (if ver = 3 then [|0;32;40;48;56;64;80;96;112;128;160;192;224;256;320;-1|] else [|0;8;16;24;32;40;48;56;64;80;96;112;128;144;160;-1|]).(bits lsr 4) in
										let srate = [|
											[|11025;-1;22050;44100|];
											[|12000;-1;24000;48000|];
											[|8000;-1;16000;32000|];
											[|-1;-1;-1;-1|]
										|].((bits lsr 2) land 2).(ver) in
										let pad = (bits lsr 1) land 1 in
										ignore(IO.read_byte i);
										let bpp = (if ver = 3 then 144 else 72) in
										let size = ((bpp * bitrate * 1000) / srate) + pad - 4 in
										ignore(IO.nread i size);
										samples := !samples + (if layer = 3 then 384 else 1152);
										read_frame()
									| _ ->
										raise Exit
								in
								read_frame();
								make_flags 2 mono sampling 16, (!samples), ("\x00\x00" ^ data)
							with Exit | IO.No_more_input | IO.Overflow _ ->
								error "Invalid MP3 file" p
							| Failure msg ->
								error ("Invalid MP3 file (" ^ msg ^ ")") p
							)
						| _ ->
							error "Sound extension not supported (only WAV or MP3)" p
					) in
					incr cid;
					classes := { f9_cid = Some !cid; f9_classname = s_type_path c.cl_path } :: !classes;
					tag (TSound { so_id = !cid; so_flags = flags; so_samples = samples; so_data = data }) :: loop l
				| _ :: l -> loop l
			in
			loop c.cl_meta
		| _ -> acc
	) [] com.types in
	let clips = [tag (TF9Classes (List.rev !classes))] in
	res @ bmp @ code @ clips

let merge com file priority (h1,tags1) (h2,tags2) =
  (* prioritize header+bgcolor for first swf *)
	let header = if priority then { h2 with h_version = max h2.h_version (swf_ver com.flash_version) } else h1 in
	let tags1 = if priority then List.filter (function { tdata = TSetBgColor _ } -> false | _ -> true) tags1 else tags1 in
  (* remove unused tags *)
	let use_stage = priority && Common.defined com "flash_use_stage" in
	let classes = ref [] in
	let nframe = ref 0 in
	let tags2 = List.filter (fun t ->
		match t.tdata with
		| TPlaceObject2 _
		| TPlaceObject3 _
		| TRemoveObject2 _
		| TRemoveObject _ -> use_stage
		| TShowFrame -> incr nframe; use_stage
		| TFilesAttributes _ | TEnableDebugger2 _ | TScenes _ -> false
		| TSetBgColor _ -> priority
		| TExport el when !nframe = 0 && com.flash_version >= 9. ->
			let el = List.filter (fun e ->
				let path = parse_path e.exp_name in
				let b = List.exists (fun t -> t_path t = path) com.types in
				if not b && fst path = [] then List.iter (fun t ->
					if snd (t_path t) = snd path then error ("Linkage name '" ^ snd path ^ "' in '" ^ file ^  "' should be '" ^ s_type_path (t_path t) ^"'") (t_infos t).mt_pos;
				) com.types;
				b
			) el in
			classes := !classes @ List.map (fun e -> { f9_cid = Some e.exp_id; f9_classname = e.exp_name }) el;
			false
		| TF9Classes el when !nframe = 0 ->
			if com.flash_version < 9. then failwith "You can't use AS3 SWF with Flash8 target";
			classes := !classes @ List.filter (fun e -> e.f9_cid <> None) el;
			false
		| _ -> true
	) tags2 in
  (* rebuild character ids *)
	let max_id = ref (-1) in
	List.iter (SwfParser.scan (fun id -> if id > !max_id then max_id := id; id) (fun id -> id)) tags1;
	incr max_id;
	let rec loop t =
		SwfParser.scan (fun id -> id + !max_id) (fun id -> id + !max_id) t;
		match t.tdata with
		| TClip c -> List.iter loop c.c_tags
		| _ -> ()
	in
	List.iter loop tags2;
	let classes = List.map (fun e -> match e.f9_cid with None -> e | Some id -> { e with f9_cid = Some (id + !max_id) }) !classes in
  (* merge timelines *)
	let rec loop l1 l2 =
		match l1, l2 with
		| ({ tdata = TSetBgColor _ } as t) :: l1, _
		| ({ tdata = TEnableDebugger2 _ } as t) :: l1, _
		| ({ tdata = TFilesAttributes _ } as t) :: l1, _ ->
			t :: loop l1 l2
		| _, ({ tdata = TSetBgColor _ } as t) :: l2 ->
			t :: loop l1 l2
		| { tdata = TShowFrame } :: l1, { tdata = TShowFrame } :: l2 ->
			tag TShowFrame :: loop l1 l2
		| { tdata = TF9Classes el } :: l1, _ ->
			(* merge all classes together *)
			tag (TF9Classes (classes @ el)) :: loop l1 l2
		| x :: l1, { tdata = TShowFrame } :: _ ->
			(* wait until we finish frame on other swf *)
			x :: loop l1 l2
		| _ , x :: l2 ->
			x :: loop l1 l2
		| x :: l1, [] ->
			x :: loop l1 l2
		| [], [] ->
			[]
	in
	let tags = loop tags1 tags2 in
	header, tags

let generate com swf_header =
	let t = Common.timer "generate swf" in
	let isf9 = com.flash_version >= 9. in
	let swc = if Common.defined com "swc" then Some (ref "") else None in
	if swc <> None && not isf9 then failwith "SWC support is only available for Flash9+";
	let file , codeclip = (try let f , c = ExtString.String.split com.file "@" in f, Some c with _ -> com.file , None) in
  (* list exports *)
	let exports = Hashtbl.create 0 in
	let toremove = ref [] in
	List.iter (fun (file,lib,_) ->
		let _, tags = lib() in
		List.iter (fun t ->
			match t.tdata with
			| TExport l -> List.iter (fun e -> Hashtbl.add exports e.exp_name ()) l
			| TF9Classes el ->
				List.iter (fun e ->
					if e.f9_cid <> None then List.iter (fun t ->
						let extern = (match t with
							| TClassDecl c -> c.cl_extern
							| TEnumDecl e -> e.e_extern
							| TAbstractDecl a -> false
							| TTypeDecl t -> false
						) in
						if not extern && s_type_path (t_path t) = e.f9_classname then
							match t with
							| TClassDecl c ->
								if has_meta ":bind" c.cl_meta then
									toremove := (t_path t) :: !toremove
								else
									error ("Class already exists in '" ^ file ^ "', use @:bind to redefine it") (t_infos t).mt_pos
							| _ ->
								error ("Invalid redefinition of class defined in '" ^ file ^ "'") (t_infos t).mt_pos
					) com.types;
				) el
			| _ -> ()
		) tags;
	) com.swf_libs;
  (* build haxe swf *)
	let tags = if isf9 then build_swf9 com file swc else build_swf8 com codeclip exports in
	let header, bg = (match swf_header with None -> default_header com | Some h -> convert_header com h) in
	let bg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
	let debug = (if isf9 && Common.defined com "fdb" then [tag (TEnableDebugger2 (0,""))] else []) in
	let fattr = (if com.flash_version < 8. then [] else
		[tag (TFilesAttributes {
			fa_network = Common.defined com "network-sandbox";
			fa_as3 = isf9;
			fa_metadata = false;
			fa_gpu = false;
			fa_direct_blt = false;
		})]
	) in
	let swf = header, fattr @ bg :: debug @ tags @ [tag TShowFrame] in
  (* merge swf libraries *)
	let priority = ref (swf_header = None) in
	let swf = List.fold_left (fun swf (file,lib,cl) ->
		let swf = merge com file !priority swf (remove_classes toremove lib cl) in
		priority := false;
		swf
	) swf com.swf_libs in
	t();
  (* write swf/swc *)
	let t = Common.timer "write swf" in
	(match swc with
	| Some cat ->
		let ch = IO.output_strings() in
		Swf.write ch swf;
		let swf = IO.close_out ch in
		let z = Zip.open_out file in
		Zip.add_entry (!cat) z "catalog.xml";
		Zip.add_entry (match swf with [s] -> s | _ -> failwith "SWF too big for SWC") z ~level:0 "library.swf";
		Zip.close_out z
	| None ->
		let ch = IO.output_channel (open_out_bin file) in
		Swf.write ch swf;
		IO.close_out ch;
	);
	t()

;;
SwfParser.init Extc.input_zip Extc.output_zip;
Swf.warnings := false;
