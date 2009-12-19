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

type context = {
	mutable f8clips : string list;
	mutable f9clips : f9class list;
	mutable code : tag_data list;
	mutable as3code : As3hl.hl_tag;
	mutable hx9code : (module_type * As3hl.hl_method * As3hl.hl_field) list;
	mutable genmethod : unit -> As3hl.hl_method;
	mutable swc_catalog : string;
}

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

let getclass i =
	if Array.length i.hls_fields <> 1 then
		None
	else match i.hls_fields.(0).hlf_kind with
	| HFClass { hlc_name = HMPath (pack,name) } -> Some (pack,name)
	| _ ->
		None

let build_movieclip ctx (pack,name) =
	let name = HMPath (pack,name) in
	let mc = HMPath (["flash";"display"],"MovieClip") in
	let c = {
		hlc_index = 0;
		hlc_name = name;
		hlc_super = Some mc;
		hlc_sealed = false;
		hlc_final = false;
		hlc_interface = false;
		hlc_namespace = None;
		hlc_implements = [||];
		hlc_construct = ctx.genmethod();
		hlc_fields = [||];
		hlc_static_construct = ctx.genmethod();
		hlc_static_fields = [||];
	} in
	let init = ctx.genmethod() in
	{
		hls_method = { init with
			hlmt_function = match init.hlmt_function with
				| None -> assert false
				| Some f -> Some { f with
					hlf_stack_size = 2;
					hlf_max_scope = 3;
					hlf_code = [|
						HThis;
						HScope;
						HGetGlobalScope;
						HGetLex mc;
						HScope;
						HGetLex mc;
						HClassDef c;
						HPopScope;
						HInitProp name;
						HRetVoid;
					|];
				}
		};
		hls_fields = [|{ hlf_name = c.hlc_name; hlf_slot = 1; hlf_kind = HFClass c; hlf_metas = None }|];
	}

let movieclip_exists types inits path =
	let name = Ast.s_type_path path in
	let rec loop i n =
		if n < 0 then false else
		match i.hls_fields.(n).hlf_kind with
		| HFClass { hlc_name = HMPath (p,n) } when (p,n) = path -> true
		| _ -> loop i (n - 1)
	in
	List.exists (function
		| TClassDecl c when c.cl_path = path ->
			let rec check_super c =
				match c.cl_super with
				| Some ({ cl_path = "flash" :: _ ,_ },_) -> ()
				| Some (c,_) -> check_super c
				| _ -> failwith ("The class " ^ name ^ " must extends a flash.* class")
			in
			check_super c;
			not c.cl_extern
		| TEnumDecl e when e.e_path = path -> failwith ("The clip " ^ name ^ " must be bound to a class")
		| TTypeDecl t when t.t_path = path -> failwith ("The clip " ^ name ^ " must be bound to a class")
		| _ -> false
	) types || 	List.exists (fun i -> loop i (Array.length i.hls_fields - 1)) inits

let add_as3_code ctx data types =
	(* set all protected+private fields to public - this will enable overriding/reflection in haXe classes *)
	let ipublic = ref (-1) in
	let ns = Array.mapi (fun i ns ->
		match ns with
		| A3NPrivate _
		| A3NInternal _
		| A3NProtected _ 
		| A3NPublic None
			->
			ipublic := i;
			A3NPublic None
		| A3NPublic _
		| A3NNamespace _
		| A3NExplicit _
		| A3NStaticProtected _ -> ns
	) data.as3_namespaces in
	let cl = Array.map (fun c ->
		{ c with cl3_namespace = None }
	) data.as3_classes in
	let data = { data with as3_namespaces = ns; as3_classes = cl } in	
	(* only keep classes that are not redefined in HX code *)
	let inits = As3hlparse.parse data in
	let inits = List.filter (fun i ->
		match getclass i with
		| None -> true
		| Some path ->
			not (List.exists (function
				| TClassDecl c -> c.cl_path = path && not c.cl_extern
				| TEnumDecl e -> e.e_path = path && not e.e_extern
				| TTypeDecl _ -> false
			) types)
	) inits in
	ctx.as3code <- ctx.as3code @ inits

let add_as3_clips ctx cl =
	ctx.f9clips <- List.filter (fun c -> c.f9_cid <> None) cl @ ctx.f9clips

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

let generate com swf_header swf_lib =
	let isf9 = com.flash_version >= 9 in
	let t = Common.timer "generate swf" in
	let file , codeclip = (try let f , c = ExtString.String.split com.file "@" in f, Some c with _ -> com.file , None) in
	let ctx = {
		f8clips = [];
		f9clips = [];
		as3code = [];
		hx9code = [];
		code = [];
		genmethod = (fun() -> assert false);
		swc_catalog = "";
	} in
	if isf9 then begin
		let code, m = Genswf9.generate com in
		ctx.f9clips <- [{ f9_cid = None; f9_classname = "flash.Boot" }];
		ctx.hx9code <- code;
		ctx.genmethod <- m;
	end else begin
		let code, clips = Genswf8.generate com in
		ctx.code <- code;
		ctx.f8clips <- List.map Ast.s_type_path clips;
	end;
	let build_swf content =
		let sandbox = (if com.flash_version >= 8 then
				let net = Common.defined com "network-sandbox" in
				[tag (TSandbox (match isf9, net with
					| true, true -> SBUnknown 9
					| true, false -> SBUnknown 8
					| _, true -> SBNetwork
					| _, false -> SBLocal
				))]
			else
				[]
		) in
		let debug = (if isf9 && Common.defined com "fdb" then [tag (TEnableDebugger2 (0,""))] else []) in
		let base_id = ref 0x5000 in
		let clips = List.fold_left (fun acc m ->
			incr base_id;
			tag ~ext:true (TClip { c_id = !base_id; c_frame_count = 1; c_tags = [] }) ::
			tag ~ext:true (TExport [{ exp_id = !base_id; exp_name = m }]) ::
			acc
		) [] ctx.f8clips in
		let code = (match codeclip with
			| None -> List.map tag ctx.code
			| Some link ->
				incr base_id;
				[
					tag (TClip {
						c_id = !base_id;
						c_frame_count = 1;
						c_tags = List.map tag ctx.code @ [tag TShowFrame];
					});
					tag (TExport [{ exp_id = !base_id; exp_name = link }]);
				]
		) in
		List.iter (fun c ->
			let path = ExtString.String.nsplit c.f9_classname "." in
			let path = (match List.rev path with [] -> assert false | x :: l -> List.rev l, x) in
			if c.f9_cid <> None && not (movieclip_exists com.types ctx.as3code path) then
				ctx.as3code <- build_movieclip ctx path :: ctx.as3code;
		) ctx.f9clips;
		let code9 = if not isf9 then [] else if Common.defined com "swc" then begin
			ctx.swc_catalog <- build_swc_catalog com (List.map (fun (t,_,_) -> t) ctx.hx9code);
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
			) ctx.hx9code
		end else begin
			let inits = List.map (fun (_,m,f) ->
				{
					hls_method = m;
					hls_fields = [|f|];
				}
			) ctx.hx9code in
			[tag (TActionScript3 (None,As3hlparse.flatten (ctx.as3code @ inits)))]
		end in
		let clips9 = (if isf9 then [tag (TF9Classes ctx.f9clips)] else []) in
		sandbox @ debug @ content @ clips @ code @ code9 @ clips9
	in
	let swf = (match swf_lib with
		| None ->
			let header , bg = (match swf_header with None -> default_header com | Some h -> convert_header com h) in
			let tagbg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
			let tagshow = tag TShowFrame in
			(header,build_swf [tagbg] @ [tagshow])
		| Some file ->
			let file = (try Common.find_file com file with Not_found -> failwith ("File not found : " ^ file)) in
			let ch = IO.input_channel (open_in_bin file) in
			let h, swf = (try Swf.parse ch with _ -> failwith ("The input swf " ^ file ^ " is corrupted")) in
			let header , tagbg = (match swf_header with
				| None ->
					{ h with h_version = com.flash_version }, None
				| Some h ->
					let h , bg = convert_header com h in
					let tagbg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
					h , Some tagbg
			) in
			IO.close_in ch;
			let rec loop acc = function
				| [] ->
					failwith ("Frame 1 not found in " ^ file)
				| t :: l ->
				match t.tdata with
				| TUnknown (0x1A,_) (*// PlaceObject2 *)
				| TUnknown (0x46,_) (*// PlaceObject3 *)
				| TPlaceObject2 _
				| TPlaceObject3 _
				| TRemoveObject2 _
				| TRemoveObject _ when not (Common.defined com "flash_use_stage") ->
					loop acc l
				| TSetBgColor _ ->
					(match tagbg with
					| None -> loop (t :: acc) l
					| Some bg -> loop (bg :: acc) l)
				| TShowFrame ->
					build_swf (List.rev acc) @ t :: l
				| TExport el ->
					if isf9 then begin
						List.iter (fun e ->
							ctx.f9clips <- { f9_cid = Some e.exp_id; f9_classname = e.exp_name } :: ctx.f9clips
						) el;
						loop acc l
					end else begin
						List.iter (fun e ->
							ctx.f8clips <- List.filter (fun x -> x <> e.exp_name) ctx.f8clips
						) el;
						loop (t :: acc) l
					end;
				| TF9Scene _
				| TEnableDebugger2 _
				| TSandbox _ ->
					loop acc l
				| TF9Classes cl ->
					if isf9 then add_as3_clips ctx cl;
					loop acc l
				| TActionScript3 (_,data) ->
					if isf9 then add_as3_code ctx data com.types;
					loop acc l
				| _ ->
					loop (t :: acc) l
			in
			(header , loop [] swf)
	) in
	t();
	let t = Common.timer "write swf" in
	if Common.defined com "swc" then begin
		let ch = IO.output_string() in
		Swf.write ch swf;
		let swf = IO.close_out ch in
		let ch = IO.output_channel (open_out_bin file) in
		let z = zip_create ch in
		zip_write_file z "catalog.xml" ctx.swc_catalog (Unix.time()) true;
		zip_write_file z "library.swf" swf (Unix.time()) false;
		zip_write_cdr z;
		IO.close_out ch;
	end else begin
		let ch = IO.output_channel (open_out_bin file) in
		Swf.write ch swf;
		IO.close_out ch;
	end;
	t();

;;
SwfParser.init SwfZip.inflate SwfZip.deflate;
SwfParser.full_parsing := false;
SwfParser.force_as3_parsing := true;
Swf.warnings := false;
