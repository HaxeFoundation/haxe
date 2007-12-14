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
open Genswf9

type context = {
	mutable f8clips : string list;
	mutable f9clips : f9class list;
	mutable code : tag_data list;
}

let debug_pass = ref ""

let tag ?(ext=false) d = {
	tid = 0;
	textended = ext;
	tdata = d;
}

let convert_header ver (w,h,fps,bg) =
	{
		h_version = ver;
		h_size = {
			rect_nbits = if (max w h) >= 820 then 16 else 15;
			left = 0;
			top = 0;
			right = w * 20;
			bottom = h * 20;
		};
		h_frame_count = 1;
		h_fps = to_float16 (if fps > 127.0 then 127.0 else fps);
		h_compressed = not (Plugin.defined "no-swf-compress");
	} , bg

let default_header ver =
	convert_header ver (400,300,30.,0xFFFFFF)

let generate file ver header infile types hres =
	let t = Plugin.timer "generate swf" in
	let file , codeclip = (try let f , c = ExtString.String.split file "@" in f, Some c with _ -> file , None) in
	let ctx = {
		f8clips = [];
		f9clips = [];
		code = [];
	} in
	if ver = 9 then begin
		let code, boot = Genswf9.generate types hres in
		ctx.code <- code;
		ctx.f9clips <- [{ f9_cid = None; f9_classname = boot }];
	end else begin
		let code, clips = Genswf8.generate file ver types hres in
		ctx.code <- code;
		ctx.f8clips <- List.map Ast.s_type_path clips;
	end;
	let build_swf content =
		let sandbox = (if ver >= 8 then 
				let net = Plugin.defined "network-sandbox" in
				[tag (TSandbox (match ver, net with
					| 9, true -> SBUnknown 9
					| 9, false -> SBUnknown 8
					| _, true -> SBNetwork
					| _, false -> SBLocal
				))]
			else
				[]
		) in
		let debug = (if ver = 9 && Plugin.defined "debug" then [tag (TEnableDebugger2 !debug_pass)] else []) in
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
		let clips9 = if ver = 9 then [tag (TF9Classes ctx.f9clips)] else [] in
		sandbox @ debug @ content @ clips @ code @ clips9
	in
	let swf = (match infile with
		| None ->
			let header , bg = (match header with None -> default_header ver | Some h -> convert_header ver h) in
			let tagbg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
			let tagshow = tag TShowFrame in
			(header,build_swf [tagbg] @ [tagshow])
		| Some file ->
			let file = (try Plugin.find_file file with Not_found -> failwith ("File not found : " ^ file)) in
			let ch = IO.input_channel (open_in_bin file) in
			let h, swf = (try Swf.parse ch with _ -> failwith ("The input swf " ^ file ^ " is corrupted")) in
			let header , tagbg = (match header with
				| None -> 
					{ h with h_version = ver }, None
				| Some h ->
					let h , bg = convert_header ver h in
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
				| TRemoveObject _ when not (Plugin.defined "flash_use_stage") ->
					loop acc l
				| TSetBgColor _ ->
					(match tagbg with
					| None -> loop (t :: acc) l
					| Some bg -> loop (bg :: acc) l)
				| TShowFrame ->
					build_swf (List.rev acc) @ t :: l
				| TExport el ->
					if ver = 9 then begin
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
					ctx.f9clips <- cl @ ctx.f9clips;
					loop acc l
				| TActionScript3 _ ->
					if ver = 9 then ctx.code <- t.tdata :: ctx.code;
					loop acc l
				| _ ->
					loop (t :: acc) l
			in
			(header , loop [] swf)
	) in
	let swf = if ver = 8 && Plugin.defined "flash_v9" then ({ (fst swf) with h_version = 9 }, snd swf) else swf in
	t();
	let t = Plugin.timer "write swf" in
	let ch = IO.output_channel (open_out_bin file) in
	Swf.write ch swf;
	IO.close_out ch;
	t();

;;
SwfParser.init SwfZip.inflate SwfZip.deflate;
SwfParser.full_parsing := false;
SwfParser.force_as3_parsing := true;
Swf.warnings := false;
