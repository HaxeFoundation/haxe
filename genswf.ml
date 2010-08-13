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

(* --- MINI ZIP IMPLEMENTATION --- *)


type zfile = {
	fname : string;
	fcompressed : bool;
	fclen : int;
	fsize : int;
	fcrc : int32;
	fdate : float;
}

type t = {
	ch : unit IO.output;
	mutable files : zfile list;
	mutable cdr_size : int;
	mutable cdr_offset : int;
}

let zip_create o = {
	ch = IO.cast_output o;
	files = [];
	cdr_size = 0;
	cdr_offset = 0;
}

let make_crc32 data =
	let init = 0xFFFFFFFFl in
	let polynom = 0xEDB88320l in
	let crc = ref init in
	for i = 0 to String.length data - 1 do
		let b = Int32.of_int (int_of_char (String.unsafe_get data i)) in
		let tmp = ref (Int32.logand (Int32.logxor (!crc) b) 0xFFl) in
		for j = 0 to 7 do
			tmp := if Int32.to_int (Int32.logand (!tmp) 1l) == 1 then
				Int32.logxor (Int32.shift_right_logical (!tmp) 1) polynom
			else
				Int32.shift_right_logical (!tmp) 1;
		done;
		crc := Int32.logxor (Int32.shift_right_logical (!crc) 8) (!tmp);
	done;
	Int32.logxor (!crc) init

let zip_write_date z d =
	let t = Unix.localtime d in
	let hour = t.Unix.tm_hour in
	let min = t.Unix.tm_min in
	let sec = t.Unix.tm_sec lsr 1 in
	IO.write_ui16 z.ch ((hour lsl 11) lor (min lsl 5) lor sec);
	let year = t.Unix.tm_year - 80 in
	let month = t.Unix.tm_mon + 1 in
	let day = t.Unix.tm_mday in
	IO.write_ui16 z.ch ((year lsl 9) lor (month lsl 5) lor day)

let zip_write_file z name data date compress =
	IO.write_i32 z.ch 0x04034B50;
	IO.write_ui16 z.ch 0x0014; (* version *)
	IO.write_ui16 z.ch 0;
	let crc32 = make_crc32 data in
	let cdata = if compress then
		let d = Extc.zip data in
		String.sub d 2 (String.length d - 4)
	else
		data
	in
	IO.write_ui16 z.ch (if compress then 0x08 else 0x00);
	zip_write_date z date;
	IO.write_real_i32 z.ch crc32;
	IO.write_i32 z.ch (String.length cdata);
	IO.write_i32 z.ch (String.length data);
	IO.write_ui16 z.ch (String.length name);
	IO.write_ui16 z.ch 0;
	IO.nwrite z.ch name;
	IO.nwrite z.ch cdata;
	z.files <- {
		fname = name;
		fcompressed = compress;
		fclen = String.length cdata;
		fsize = String.length data;
		fcrc = crc32;
		fdate = date;
	} :: z.files

let zip_write_cdr_file z f =
	let namelen = String.length f.fname in
	IO.write_i32 z.ch 0x02014B50;
	IO.write_ui16 z.ch 0x0014;
	IO.write_ui16 z.ch 0x0014;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch (if f.fcompressed then 0x08 else 0);
	zip_write_date z f.fdate;
	IO.write_real_i32 z.ch f.fcrc;
	IO.write_i32 z.ch f.fclen;
	IO.write_i32 z.ch f.fsize;
	IO.write_ui16 z.ch namelen;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch 0;
	IO.write_i32 z.ch 0;
	IO.write_i32 z.ch z.cdr_offset;
	IO.nwrite z.ch f.fname;
	z.cdr_size <- z.cdr_size + 46 + namelen;
	z.cdr_offset <- z.cdr_offset + 30 + namelen + f.fclen

let zip_write_cdr z =
	List.iter (zip_write_cdr_file z) (List.rev z.files);
	IO.write_i32 z.ch 0x06054B50;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch (List.length z.files);
	IO.write_ui16 z.ch (List.length z.files);
	IO.write_i32 z.ch z.cdr_size;
	IO.write_i32 z.ch z.cdr_offset;
	IO.write_ui16 z.ch 0

(* ------------------------------- *)

let rec make_tpath = function
	| HMPath (pack,name) ->
		let pdyn = ref false in
		let pack, name = match pack, name with
			| [], "void" -> [], "Void"
			| [], "int" -> [], "Int"
			| [], "uint" -> [], "UInt"
			| [], "Number" -> [], "Float"
			| [], "Boolean" -> [], "Bool"
			| [], "Object" | [], "Function" -> [], "Dynamic"
			| [],"Class" | [],"Array" -> pdyn := true; pack, name
			| _ -> pack, name
		in
		{
			tpackage = pack;
			tname = name;
			tparams = if !pdyn then [TPType (TPNormal { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None; })] else[];
			tsub = None;
		}
	| HMName (id,_) ->
		{
			tpackage = [];
			tname = id;
			tparams = [];
			tsub = None;
		}
	| HMMultiName (Some id,[HNPublic (Some ns)]) ->
		{
			tpackage = ExtString.String.nsplit ns ".";
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
	| HMParams (t,params) ->
		let params = List.map (fun t -> TPType (TPNormal (make_tpath t))) params in
		{ (make_tpath t) with tparams = params }

let make_param cl p =
	{ tpackage = fst cl; tname = snd cl; tparams = [TPType (TPNormal { tpackage = fst p; tname = snd p; tparams = []; tsub = None })]; tsub = None }

let make_topt = function
	| None -> { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None }
	| Some t -> make_tpath t

let make_type f t = 
	TPNormal (match f, t with
	| "opaqueBackground", Some (HMPath ([],"Object")) -> make_param ([],"Null") ([],"UInt")
	| "getObjectsUnderPoint", Some (HMPath ([],"Array")) -> make_param ([],"Array") (["flash";"display"],"DisplayObject")
	| "blendMode", Some (HMPath ([],"String")) -> { tpackage = ["flash";"display"]; tname = "BlendMode"; tparams = []; tsub = None }
	| _ -> make_topt t)

let build_class com c file =
	let path = make_tpath c.hlc_name in
  (* make flags *)
	let flags = [HExtern] in
	let flags = if c.hlc_interface then HInterface :: flags else flags in
	let flags = (match c.hlc_super with
		| None | Some (HMPath ([],"Object")) -> flags
		| Some s -> HExtends (make_tpath s) :: flags
	) in
	let flags = List.map (fun i -> HImplements (make_tpath i)) (Array.to_list c.hlc_implements) @ flags in
	let flags = if c.hlc_sealed || Common.defined com "flash_strict" then flags else HImplements (make_tpath (HMPath ([],"Dynamic"))) :: flags in
  (* make fields *)
	let pos = { pfile = file ^ "@" ^ s_type_path (path.tpackage,path.tname); pmin = 0; pmax = 0 } in
	let getters = Hashtbl.create 0 in
	let setters = Hashtbl.create 0 in
	let as3_native = Common.defined com "as3_native" in
	let make_field stat acc f =
		let meta = ref None in
		let flags = (match f.hlf_name with
			| HMPath _ -> [APublic]
			| HMName (_,ns) ->
				(match ns with
				| HNPrivate _ | HNNamespace "http://www.adobe.com/2006/flex/mx/internal" -> []
				| HNNamespace ns ->
					meta := Some (":ns",[String ns]);
					[APublic]
				| HNExplicit _ | HNInternal _ | HNPublic _ ->
					[APublic]
				| HNStaticProtected _ | HNProtected _ ->
					if as3_native then meta := Some (":protected",[]);
					[APrivate])
			| _ -> []
		) in
		if flags = [] then acc else
		let flags = if stat then AStatic :: flags else flags in
		let meta = (match !meta with None -> [] | Some (s,cl) -> [s,List.map (fun c -> EConst c,pos) cl]) in
		let name = (make_tpath f.hlf_name).tname in
		match f.hlf_kind with
		| HFVar v ->
			let v = if v.hlv_const then
				FProp (name,None,meta,flags,"default","never",make_type name v.hlv_type)
			else
				FVar (name,None,meta,flags,Some (make_type name v.hlv_type),None)
			in
			v :: acc
		| HFMethod m when not m.hlm_override ->
			(match m.hlm_kind with
			| MK3Normal ->
				let t = m.hlm_type in
				let p = ref 0 in
				let args = List.map (fun at ->
					let aname = (match t.hlmt_pnames with
						| None -> "p" ^ string_of_int !p
						| Some l ->
							match List.nth l !p with
							| None -> "p" ^ string_of_int !p
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
					(aname,opt_val <> None,Some (make_type name at),None)
				) t.hlmt_args in
				let f = {
					f_args = args;
					f_type = Some (make_type name t.hlmt_ret);
					f_expr = (EBlock [],pos)
				} in
				FFun (name,None,meta,flags,[],f) :: acc
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
			| Some t1, Some t2 -> if t1 <> t2 then assert false; true, true, t1
		) in
		let flags = [APublic] in
		let flags = if stat then AStatic :: flags else flags in
		FProp (name,None,[],flags,(if get then "default" else "never"),(if set then "default" else "never"),make_type name t)
	in
	let fields = Hashtbl.fold (fun (name,stat) t acc ->
		make_get_set name stat (Some t) (try Some (Hashtbl.find setters (name,stat)) with Not_found -> None) :: acc
	) getters fields in
	let fields = Hashtbl.fold (fun (name,stat) t acc ->
		if Hashtbl.mem getters (name,stat) then
			acc
		else
			make_get_set name stat None (Some t) :: acc
	) setters fields in
	let class_data = {
		d_name = path.tname;
		d_doc = None;
		d_params = [];
		d_meta = [];
		d_flags = flags;
		d_data = List.map (fun f -> f, pos) fields;
	} in
	(path.tpackage, [(EClass class_data,pos)])

let extract_data swf =
	let cache = ref None in
	(fun() ->
		match !cache with
		| Some h -> h
		| None ->
			let _, tags = swf() in
			let t = Common.timer "read swf" in
			let h = Hashtbl.create 0 in
			let rec loop_field f =
				match f.hlf_kind with
				| HFClass c ->
					let path = make_tpath f.hlf_name in
					Hashtbl.add h (path.tpackage,path.tname) c
				| _ -> ()
			in
			List.iter (fun t ->
				match t.tdata with
				| TActionScript3 (_,as3) ->
					List.iter (fun i -> Array.iter loop_field i.hls_fields) (As3hlparse.parse as3)
				| _ -> ()
			) tags;
			cache := Some h;
			t();
			h)

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
	let data = ref None in
	(fun () ->
		match !data with
		| Some swf -> swf
		| None ->
			let t = Common.timer "read swf" in
			let file = (try Common.find_file com file with Not_found -> failwith ("SWF Library not found : " ^ file)) in
			let ch = IO.input_channel (open_in_bin file) in
			let h, tags = (try Swf.parse ch with _ -> failwith ("The input swf " ^ file ^ " is corrupted")) in
			IO.close_in ch;
			List.iter (fun t ->
				match t.tdata with
				| TActionScript3 (id,as3) when not com.debug && not !Common.display ->					
					t.tdata <- TActionScript3 (id,remove_debug_infos as3)
				| _ -> ()
			) tags;
			t();
			data := Some (h,tags);
			(h,tags))

(* ------------------------------- *)

let tag ?(ext=false) d = {
	tid = 0;
	textended = ext;
	tdata = d;
}

let convert_header com (w,h,fps,bg) =
	if max w h >= 1639 then failwith "-swf-header : size too large";
	{
		h_version = com.flash_version;
		h_size = {
			rect_nbits = if (max w h) >= 820 then 16 else 15;
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
			List.iter (fun (_,_,t) -> add_type t) f.tf_args;
			add_type f.tf_type;
			add_expr f.tf_expr;
		| TFor (_,t,e1,e2) ->
			add_type t;
			add_expr e1;
			add_expr e2;
		| TVars vl ->
			List.iter (fun (_,t,e) ->
				add_type t;
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

let make_as3_public data =
	(* set all protected+private fields to public - this will enable overriding/reflection in haXe classes *)
	let ns = Array.mapi (fun i ns ->
		match ns with
		| A3NPrivate _
		| A3NInternal _
		| A3NProtected _
		| A3NPublic None
			->
			A3NPublic None
		| A3NPublic _
		| A3NNamespace _
		| A3NExplicit _
		| A3NStaticProtected _ -> ns
	) data.as3_namespaces in
	let cl = Array.map (fun c -> { c with cl3_namespace = None }) data.as3_classes in
	{ data with as3_namespaces = ns; as3_classes = cl }

let remove_classes toremove lib hcl =
	let lib = lib() in
	match !toremove with
	| [] -> lib
	| _ ->
		let hcl = hcl() in
		match List.filter (fun c -> Hashtbl.mem hcl c) (!toremove) with
		| [] -> lib
		| classes ->
			let rec loop t =
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
					{ t with tdata = TActionScript3 (h,As3hlparse.flatten data) }
				| _ -> t
			in
			toremove := List.filter (fun p -> not (List.mem p classes)) !toremove;
			fst lib, List.map loop (snd lib)

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

let build_swf9 com swc =
	let code = Genswf9.generate com in
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
	let clips = [tag (TF9Classes [{ f9_cid = None; f9_classname = "flash.Boot" }])] in
	code @ clips

let merge com file priority (h1,tags1) (h2,tags2) =
  (* prioritize header+bgcolor for first swf *)
	let header = if priority then { h2 with h_version = max h2.h_version com.flash_version } else h1 in
	let tags1 = if priority then List.filter (function { tdata = TSetBgColor _ } -> false | _ -> true) tags1 else tags1 in
  (* remove unused tags *)
	let use_stage = priority && Common.defined com "flash_use_stage" in
	let as3_native = Common.defined com "as3_native" in
	let classes = ref [] in
	let nframe = ref 0 in
	let tags2 = List.filter (fun t ->
		match t.tdata with
		| TPlaceObject2 _
		| TPlaceObject3 _
		| TRemoveObject2 _
		| TRemoveObject _ -> use_stage
		| TShowFrame -> incr nframe; use_stage
		(* patch : this class has a public method which redefines a private one ! *)
		| TActionScript3 (Some (_,"org/papervision3d/render/QuadrantRenderEngine"),_) when not as3_native -> false
		| TFilesAttributes _ | TEnableDebugger2 _ | TScenes _ -> false
		| TSetBgColor _ -> priority
		| TExport el when !nframe = 0 && com.flash_version >= 9 ->
			let el = List.filter (fun e ->
				let path = (match List.rev (ExtString.String.nsplit e.exp_name ".") with [] -> assert false | name :: l -> List.rev l, name) in
				List.exists (fun t -> t_path t = path) com.types
			) el in
			classes := !classes @ List.map (fun e -> { f9_cid = Some e.exp_id; f9_classname = e.exp_name }) el;
			false
		| TF9Classes el when !nframe = 0 ->
			if com.flash_version < 9 then failwith "You can't use AS3 SWF with Flash8 target";
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
  (* do additional transforms *)
	let tags2 = List.map (fun t ->
		match t.tdata with
		| TActionScript3 (id,data) when not as3_native -> { t with tdata = TActionScript3 (id,make_as3_public data) }
		| _ -> t
	) tags2 in
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
	let isf9 = com.flash_version >= 9 in
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
							| TTypeDecl t -> false
						) in
						if not extern && s_type_path (t_path t) = e.f9_classname then
							match t with
							| TClassDecl c ->
								if has_meta ":bind" c.cl_meta then
									toremove := (t_path t) :: !toremove
								else
									error ("Class already exists in '" ^ file ^ "', use @:bind to redefine it") (t_pos t)
							| _ ->
								error ("Invalid redefinition of class defined in '" ^ file ^ "'") (t_pos t)						
					) com.types;
				) el
			| _ -> ()
		) tags;
	) com.swf_libs;
  (* build haxe swf *)
	let tags = if isf9 then build_swf9 com swc else build_swf8 com codeclip exports in
	let header, bg = (match swf_header with None -> default_header com | Some h -> convert_header com h) in
	let bg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
	let debug = (if isf9 && Common.defined com "fdb" then [tag (TEnableDebugger2 (0,""))] else []) in
	let fattr = (if com.flash_version < 8 then [] else
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
		let ch = IO.output_channel (open_out_bin file) in
		let z = zip_create ch in
		zip_write_file z "catalog.xml" (!cat) (Unix.time()) true;
		zip_write_file z "library.swf" (match swf with [s] -> s | _ -> failwith "SWF too big for SWC") (Unix.time()) false;
		zip_write_cdr z;
		IO.close_out ch;
	| None ->
		let ch = IO.output_channel (open_out_bin file) in
		Swf.write ch swf;
		IO.close_out ch;
	);
	t()

;;
SwfParser.init Extc.input_zip Extc.output_zip;
Swf.warnings := false;
