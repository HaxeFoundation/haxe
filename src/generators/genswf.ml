(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
open Swf
open As3hl
open Genswf9
open ExtString
open Type
open Common
open Ast
open Globals

let tag ?(ext=false) d = {
	tid = 0;
	textended = ext;
	tdata = d;
}

let convert_header com (w,h,fps,bg) =
	let high = (max w h) * 20 in
	let rec loop b =
		if 1 lsl b > high then b else loop (b + 1)
	in
	let bits = loop 0 in
	{
		h_version = Common.flash_version_tag com.flash_version;
		h_size = {
			rect_nbits = bits + 1;
			left = 0;
			top = 0;
			right = w * 20;
			bottom = h * 20;
		};
		h_frame_count = 1;
		h_fps = to_float16 (if fps > 127.0 then 127.0 else fps);
		h_compressed = not (Common.defined com Define.NoSwfCompress);
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
			(match c.cl_kind with KTypeParameter _ -> () | _ -> add_path c.cl_path DKType);
			List.iter (add_type_rec (t::l)) pl;
		| TAbstract (a,pl) ->
			if Meta.has Meta.CoreType a.a_meta then
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
			add_type_rec l (lazy_type f)
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
		| TVar (v,eo) ->
				add_type v.v_type;
			begin match eo with
				| None -> ()
				| Some e -> add_expr e
			end
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
		) c.cl_params;
		List.iter add_inherit c.cl_implements;
	| TEnumDecl e when not e.e_extern ->
		PMap.iter (fun _ f -> add_type f.ef_type) e.e_constrs;
	| _ -> ());
	h := PMap.remove (([],"Int"),DKType) (!h);
	h := PMap.remove (([],"Int"),DKExpr) (!h);
	h := PMap.remove (([],"Void"),DKType) (!h);
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
			node "haxe" ["version",Printf.sprintf "%d.%.2d" (com.version/10000) (com.version mod 10000)] [];
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

type file_format =
	| BJPG
	| BPNG
	| BGIF
	| SWAV
	| SMP3

let detect_format data p =
	match (try data.[0],data.[1],data.[2] with _ -> '\x00','\x00','\x00') with
	| '\xFF', '\xD8', _ -> BJPG
	| '\x89', 'P', 'N' -> BPNG
	| 'R', 'I', 'F' -> SWAV
	| 'I', 'D', '3' -> SMP3
	| '\xFF', i, _ when (int_of_char i) land 0xE2 = 0xE2 -> SMP3
	| 'G', 'I', 'F' -> BGIF
	| _ ->
		abort "Unknown file format" p

open TTFData

let build_swf9 com file swc =
	let boot_name = if swc <> None || Common.defined com Define.HaxeBoot then "haxe" else "boot_" ^ (String.sub (Digest.to_hex (Digest.string (Filename.basename file))) 0 4) in
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
		[tag (TActionScript3 ((if Common.defined com Define.SwfUseDoAbc then Some(1,boot_name) else None), As3hlparse.flatten inits))]
	) in
	let cid = ref 0 in
	let classes = ref [{ f9_cid = None; f9_classname = boot_name }] in
	let res = Hashtbl.fold (fun name data acc ->
		incr cid;
		classes := { f9_cid = Some !cid; f9_classname = s_type_path (Genswf9.resource_path name) } :: !classes;
		tag (TBinaryData (!cid,data)) :: acc
	) com.resources [] in
	let load_file_data file p =
		let file = try Common.find_file com file with Not_found -> file in
		if String.length file > 5 && String.sub file 0 5 = "data:" then
			String.sub file 5 (String.length file - 5)
		else
			(try Std.input_file ~bin:true file with Invalid_argument _ -> abort "File is too big (max 16MB allowed)" p | _  -> abort "File not found" p)
	in
	let bmp = List.fold_left (fun acc t ->
		match t with
		| TClassDecl c ->
			let rec loop = function
				| [] -> acc
				| (Meta.Font,(EConst (String (file,_)),p) :: args,_) :: l ->
					let file = try Common.find_file com file with Not_found -> file in
					let ch = try open_in_bin file with _ -> abort "File not found" p in
					let ttf = try TTFParser.parse ch with e -> abort ("Error while parsing font " ^ file ^ " : " ^ Printexc.to_string e) p in
					close_in ch;
					let get_string e = match fst e with
						| EConst (String (s,_)) -> Some s
						| _ -> raise Not_found
					in
					let ttf_config = {
						ttfc_range_str = "";
						ttfc_font_name = None;
					} in
					begin match args with
						| (EConst (String (str,_)),_) :: _ -> ttf_config.ttfc_range_str <- str;
						| _ -> ()
					end;
					begin match args with
						| _ :: [e] ->
							begin match fst e with
								| EObjectDecl fl ->
									begin try ttf_config.ttfc_font_name <- get_string (Expr.field_assoc "fontName" fl)
									with Not_found -> () end
								| _ ->
									()
							end
						| _ ->
							()
					end;
					let ttf_swf = TTFSwfWriter.to_swf ttf ttf_config in
					let ch = IO.output_string () in
					let b = IO.output_bits ch in
					TTFSwfWriter.write_font2 ch b ttf_swf;
					let data = IO.close_out ch in
					incr cid;
					classes := { f9_cid = Some !cid; f9_classname = s_type_path c.cl_path } :: !classes;
					tag (TFont3 {
						cd_id = !cid;
						cd_data = data;
					}) :: loop l
				| (Meta.Bitmap,[EConst (String (file,_)),p],_) :: l ->
					let data = load_file_data file p in
					incr cid;
					classes := { f9_cid = Some !cid; f9_classname = s_type_path c.cl_path } :: !classes;
					let raw() =
						tag (TBitsJPEG2 { bd_id = !cid; bd_data = data; bd_table = None; bd_alpha = None; bd_deblock = Some 0 })
					in
					let t = (match detect_format data p with
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
									if h.Png.png_width * h.Png.png_height * 4 > Sys.max_string_length then begin
										com.warning "Flash will loose some color information for this file, add alpha channel to preserve it" p;
										raise Exit;
									end;
									let data = Extc.unzip (Png.data png) in
									let raw_data = Png.filter png data in
									let cmp_data = Extc.zip raw_data in
									tag ~ext:true (TBitsLossless2 { bll_id = !cid; bll_format = 5; bll_width = h.Png.png_width; bll_height = h.Png.png_height; bll_data = cmp_data })
								| _ -> raw())
							with Exit ->
								raw()
							| _ ->
								com.error ("Failed to decode this PNG " ^ file) p;
								raw();
							)
						| _ -> raw()
					) in
					t :: loop l
				| (Meta.Bitmap,[EConst (String (dfile,_)),p1;EConst (String (afile,_)),p2],_) :: l ->
					let ddata = load_file_data dfile p1 in
					let adata = load_file_data afile p2 in
					(match detect_format ddata p1 with
					| BJPG -> ()
					| _ -> abort "RGB channel must be a JPG file" p1);
					(match detect_format adata p2 with
					| BPNG -> ()
					| _ -> abort "Alpha channel must be a PNG file" p2);
					let png = Png.parse (IO.input_string adata) in
					let h = Png.header png in
					let amask = (match h.Png.png_color with
						| Png.ClTrueColor (Png.TBits8,Png.HaveAlpha) ->
							let data = Extc.unzip (Png.data png) in
							let raw_data = Png.filter png data in
							let alpha = Bytes.make (h.Png.png_width * h.Png.png_height) '\000' in
							for i = 0 to Bytes.length alpha do
								Bytes.unsafe_set alpha i (String.unsafe_get raw_data (i lsl 2));
							done;
							Extc.zip (Bytes.unsafe_to_string alpha)
						| _ -> abort "PNG file must contain 8 bit alpha channel" p2
					) in
					incr cid;
					classes := { f9_cid = Some !cid; f9_classname = s_type_path c.cl_path } :: !classes;
					tag (TBitsJPEG3 { bd_id = !cid; bd_data = ddata; bd_table = None; bd_alpha = Some amask; bd_deblock = Some 0 }) :: loop l
				| (Meta.File,[EConst (String (file,_)),p],_) :: l ->
					let data = load_file_data file p in
					incr cid;
					classes := { f9_cid = Some !cid; f9_classname = s_type_path c.cl_path } :: !classes;
					tag (TBinaryData (!cid,data)) :: loop l
				| (Meta.Sound,[EConst (String (file,_)),p],_) :: l ->
					let data = load_file_data file p in
					let make_flags fmt mono freq bits =
						let fbits = (match freq with 5512 when fmt <> 2 -> 0 | 11025 -> 1 | 22050 -> 2 | 44100 -> 3 | _ -> failwith ("Unsupported frequency " ^ string_of_int freq)) in
						let bbits = (match bits with 8 -> 0 | 16 -> 1 | _ -> failwith ("Unsupported bits " ^ string_of_int bits)) in
						(fmt lsl 4) lor (fbits lsl 2) lor (bbits lsl 1) lor (if mono then 0 else 1)
					in
					let flags, samples, data = (match detect_format data p with
						| SWAV ->
							(try
								let i = IO.input_string data in
								if IO.nread_string i 4 <> "RIFF" then raise Exit;
								ignore(IO.nread i 4); (* size *)
								if IO.nread_string i 4 <> "WAVE" || IO.nread_string i 4 <> "fmt " then raise Exit;
								let chunk_size = IO.read_i32 i in
								if not (chunk_size = 0x10 || chunk_size = 0x12 || chunk_size = 0x40) then failwith ("Unsupported chunk size " ^ string_of_int chunk_size);
								if IO.read_ui16 i <> 1 then failwith "Not a PCM file";
								let chan = IO.read_ui16 i in
								if chan > 2 then failwith "Too many channels";
								let freq = IO.read_i32 i in
								ignore(IO.read_i32 i);
								ignore(IO.read_i16 i);
								let bits = IO.read_ui16 i in
								if chunk_size <> 0x10 then ignore(IO.nread i (chunk_size - 0x10));
								if IO.nread_string i 4 <> "data" then raise Exit;
								let data_size = IO.read_i32 i in
								let data = IO.nread_string i data_size in
								make_flags 0 (chan = 1) freq bits, (data_size * 8 / (chan * bits)), data
							with Exit | IO.No_more_input | IO.Overflow _ ->
								abort "Invalid WAV file" p
							| Failure msg ->
								abort ("Invalid WAV file (" ^ msg ^ ")") p
							)
						| SMP3 ->
							(try
								let sampling = ref 0 in
								let mono = ref false in
								let samples = ref 0 in
								let i = IO.input_string data in
								let rec read_frame() =
									match (try IO.read_byte i with IO.No_more_input -> -1) with
									| -1 ->
										()
									| 0x49 ->
										(* ID3 *)
										if IO.nread_string i 2 <> "D3" then raise Exit;
										ignore(IO.read_ui16 i); (* version *)
										ignore(IO.read_byte i); (* flags *)
										let size = IO.read_byte i land 0x7F in
										let size = size lsl 7 lor (IO.read_byte i land 0x7F) in
										let size = size lsl 7 lor (IO.read_byte i land 0x7F) in
										let size = size lsl 7 lor (IO.read_byte i land 0x7F) in
										ignore(IO.nread i size); (* id3 data *)
										read_frame()
									| 0x54 ->
										(* TAG and TAG+ *)
										if IO.nread_string i 3 = "AG+" then ignore(IO.nread i 223) else ignore(IO.nread i 124);
										read_frame()
									| 0xFF ->
										let infos = IO.read_byte i in
										let ver = (infos lsr 3) land 3 in
										sampling := [|11025;0;22050;44100|].(ver);
										let layer = (infos lsr 1) land 3 in
										let bits = IO.read_byte i in
										let bitrate = (if ver = 3 then [|0;32;40;48;56;64;80;96;112;128;160;192;224;256;320;-1|] else [|0;8;16;24;32;40;48;56;64;80;96;112;128;144;160;-1|]).(bits lsr 4) in
										let srate = [|
											[|11025;12000;8000;-1|];
											[|-1;-1;-1;-1|];
											[|22050;24000;16000;-1|];
											[|44100;48000;32000;-1|];
										|].(ver).((bits lsr 2) land 3) in
										let pad = (bits lsr 1) land 1 in
										mono := (IO.read_byte i) lsr 6 = 3;
										let bpp = (if ver = 3 then 144 else 72) in
										let size = ((bpp * bitrate * 1000) / srate) + pad - 4 in
										ignore(IO.nread i size);
										samples := !samples + (if layer = 3 then 384 else 1152);
										read_frame()
									| _ ->
										raise Exit
								in
								read_frame();
								make_flags 2 !mono !sampling 16, (!samples), ("\x00\x00" ^ data)
							with Exit | IO.No_more_input | IO.Overflow _ ->
								abort "Invalid MP3 file" p
							| Failure msg ->
								abort ("Invalid MP3 file (" ^ msg ^ ")") p
							)
						| _ ->
							abort "Sound extension not supported (only WAV or MP3)" p
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
	let header = if priority then { h2 with h_version = max h2.h_version (Common.flash_version_tag com.flash_version) } else h1 in
	let tags1 = if priority then List.filter (function { tdata = TSetBgColor _ } -> false | _ -> true) tags1 else tags1 in
	(* remove unused tags *)
	let use_stage = priority && Common.defined com Define.FlashUseStage in
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
		| TMetaData _ -> not (Common.defined com Define.SwfMetadata)
		| TSetBgColor _ -> priority
		| TExport el when !nframe = 0 && com.flash_version >= 9. ->
			let el = List.filter (fun e ->
				let path = parse_path e.exp_name in
				let b = List.exists (fun t -> t_path t = path) com.types in
				if not b && fst path = [] then List.iter (fun t ->
					if snd (t_path t) = snd path then abort ("Linkage name '" ^ snd path ^ "' in '" ^ file ^  "' should be '" ^ s_type_path (t_path t) ^"'") (t_infos t).mt_pos;
				) com.types;
				b
			) el in
			classes := !classes @ List.map (fun e -> { f9_cid = Some e.exp_id; f9_classname = e.exp_name }) el;
			false
		| TF9Classes el when !nframe = 0 ->
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

let generate swf_header com =
	let swc = if Common.defined com Define.Swc then Some (ref "") else None in
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
								if Meta.has Meta.Bind c.cl_meta then
									toremove := (t_path t) :: !toremove
								else
									abort ("Class already exists in '" ^ file ^ "', use @:bind to redefine it") (t_infos t).mt_pos
							| _ ->
								abort ("Invalid redefinition of class defined in '" ^ file ^ "'") (t_infos t).mt_pos
					) com.types;
				) el
			| _ -> ()
		) tags;
	) com.swf_libs;
	(* build haxe swf *)
	let tags = build_swf9 com file swc in
	let header, bg = (match swf_header with None -> default_header com | Some h -> convert_header com h) in
	let bg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
	let scene = tag ~ext:true (TScenes ([(0,"Scene1")],[])) in
	let swf_debug_password = try
		Digest.to_hex(Digest.string (Common.defined_value com Define.SwfDebugPassword))
	with Not_found ->
		""
	in
	let debug = (if Common.defined com Define.Fdb then [tag (TEnableDebugger2 (0, swf_debug_password))] else []) in
	let meta_data =
		try
			let file = Common.defined_value com Define.SwfMetadata in
			let file = try Common.find_file com file with Not_found -> file in
			let data = try Std.input_file ~bin:true file with Sys_error _ -> failwith ("Metadata resource file not found : " ^ file) in
			[tag(TMetaData (data))]
		with Not_found ->
			[]
	in
	let fattr = (if com.flash_version < 8. then [] else
		[tag (TFilesAttributes {
			fa_network = Common.defined com Define.NetworkSandbox;
			fa_as3 = true;
			fa_metadata = meta_data <> [];
			fa_gpu = com.flash_version > 9. && Common.defined com Define.SwfGpu;
			fa_direct_blt = com.flash_version > 9. && Common.defined com Define.SwfDirectBlit;
		})]
	) in
	let fattr = if Common.defined com Define.AdvancedTelemetry then fattr @ [tag (TUnknown (0x5D,"\x00\x00"))] else fattr in
	let swf_script_limits = try
		let s = Common.defined_value com Define.SwfScriptTimeout in
		let i = try int_of_string s with _ -> abort "Argument to swf_script_timeout must be an integer" null_pos in
		[tag(TScriptLimits (256, if i < 0 then 0 else if i > 65535 then 65535 else i))]
	with Not_found ->
		[]
	in
	let swf = header, fattr @ meta_data @ bg :: scene :: debug @ swf_script_limits @ tags @ [tag TShowFrame] in
	(* merge swf libraries *)
	let priority = ref (swf_header = None) in
	let swf = List.fold_left (fun swf (file,lib,cl) ->
		let swf = merge com file !priority swf (SwfLoader.remove_classes toremove lib cl) in
		priority := false;
		swf
	) swf com.swf_libs in
	let swf = match swf with
	| header,tags when Common.defined com Define.SwfPreloaderFrame ->
		let rec loop l =
			match l with
			| ({tdata = TFilesAttributes _ | TUnknown (0x5D,"\x00\x00") | TMetaData _ | TSetBgColor _ | TEnableDebugger2 _ | TScriptLimits _} as t) :: l -> t :: loop l
			| t :: l -> tag TShowFrame :: t :: l
			| [] -> []
		in
		{header with h_frame_count = header.h_frame_count + 1},loop tags
	| _ -> swf in
	(* write swf/swc *)
	let t = Common.timer ["write";"swf"] in
	let level = (try int_of_string (Common.defined_value com Define.SwfCompressLevel) with Not_found -> 9) in
	SwfParser.init Extc.input_zip (Extc.output_zip ~level);
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
