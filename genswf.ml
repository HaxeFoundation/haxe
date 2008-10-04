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
	mutable hx9code : As3hl.hl_tag;
	mutable genmethod : unit -> As3hl.hl_method;
}

let tag ?(ext=false) d = {
	tid = 0;
	textended = ext;
	tdata = d;
}

let convert_header com (w,h,fps,bg) =
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
	} in
	if isf9 then begin
		let code, boot, m = Genswf9.generate com in
		ctx.hx9code <- (match code with
			| [i] when Array.length i.hls_fields = 0 ->
				(* if we don't have any class defined, don't include Boot *)
				[]
			| _ ->
				ctx.f9clips <- [{ f9_cid = None; f9_classname = boot }];
				code
		);
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
		let as3code = (match ctx.as3code @ ctx.hx9code with [] -> [] | l -> [tag (TActionScript3 (None,As3hlparse.flatten l))]) in
		let clips9 = (if isf9 then [tag (TF9Classes ctx.f9clips)] else []) in
		sandbox @ debug @ content @ clips @ code @ as3code @ clips9
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
	let ch = IO.output_channel (open_out_bin file) in
	Swf.write ch swf;
	IO.close_out ch;
	t();

;;
SwfParser.init SwfZip.inflate SwfZip.deflate;
SwfParser.full_parsing := false;
SwfParser.force_as3_parsing := true;
Swf.warnings := false;
