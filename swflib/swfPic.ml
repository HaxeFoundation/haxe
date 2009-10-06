(*
 *  This file is part of SwfLib
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

open Png
open Swf
open ExtList

type error_msg =
	| PngError of Png.error_msg
	| Interlaced
	| UnsupportedColorModel
	| UnsupportedExtension
	| UnzipFailed

exception Error of error_msg
exception File_not_found of string

type picture = {
	pwidth : int;
	pheight : int;
	pid : int;
	pdata : tag_data;
	pframe : string option;
}

let error_msg = function
	| PngError m -> Png.error_msg m
	| Interlaced -> "Interlaced mode is not supported"
	| UnsupportedColorModel -> "Unsupported color model"
	| UnsupportedExtension -> "Unsupported file extension"
	| UnzipFailed -> "Decompression failed"

let error msg = raise (Error msg)

let unsigned v n =
	if v < 0 then
		(- ( v + 1 )) lxor (1 lsl n - 1)
	else
		v

let load_picture file id =
	let ch = IO.input_channel (try open_in_bin file with _ -> raise (File_not_found file)) in
	let len = String.length file in
	let p = (try String.rindex file '.' with Not_found -> len) in
	let ext = String.sub file (p + 1) (len - (p + 1)) in
	match String.uppercase ext with
	| "PNG" ->
		let png , header, data = (try
			let p = Png.parse ch in
			p , Png.header p, Png.data p
		with Png.Error msg ->
			IO.close_in ch; error (PngError msg)
		) in
		IO.close_in ch;
		if header.png_interlace then error Interlaced;
		let data = (try Extc.unzip data with _ -> error UnzipFailed) in
		let w = header.png_width in
		let h = header.png_height in
		let data = (try Png.filter png data with Png.Error msg -> error (PngError msg)) in
		{
			pwidth = w;
			pheight = h;
			pid = id;
			pdata = (match header.png_color with
				| ClTrueColor (TBits8,NoAlpha) ->
					(* set alpha to 0 *)
					for p = 0 to w * h - 1 do
						String.unsafe_set data (p * 4) '\000';
					done;
					TBitsLossless {
						bll_id = id;
						bll_format = 5;
						bll_width = w;
						bll_height = h;
						bll_data = Extc.zip data;
					}
				| ClTrueColor (TBits8,HaveAlpha) ->
					(* premultiply rgb by alpha *)
					for p = 0 to w * h - 1 do
						let k = p * 4 in
						let a = int_of_char (String.unsafe_get data k) in
						String.unsafe_set data (k + 1) (Char.unsafe_chr ((int_of_char (String.unsafe_get data (k + 1)) * a) / 0xFF));
						String.unsafe_set data (k + 2) (Char.unsafe_chr ((int_of_char (String.unsafe_get data (k + 2)) * a) / 0xFF));
						String.unsafe_set data (k + 3) (Char.unsafe_chr ((int_of_char (String.unsafe_get data (k + 3)) * a) / 0xFF));
					done;
					TBitsLossless2 {
						bll_id = id;
						bll_format = 5;
						bll_width = w;
						bll_height = h;
						bll_data = Extc.zip data;
					}
				| _ -> error UnsupportedColorModel);
		}
	| _ ->
		IO.close_in ch;
		error UnsupportedExtension

let make_clip name pics baseid =
	let npics = List.length pics in
	let ids = Array.of_list (List.map (fun p -> p.pid) pics) in
	let rec loop i p =
		let w = p.pwidth in
		let h = p.pheight in
		let rb = if 20 * max w h >= 1 lsl 14 then 15 else 14 in
		let nbits = rb in
		TShape {
			sh_id = baseid + i;
			sh_bounds = {
				rect_nbits = rb;
				left = 0;
				top = 0;
				right = w * 20;
				bottom = h * 20;
			};
			sh_bounds2 = None;
			sh_style = {
				sws_fill_styles = [
					SFSBitmap {
						sfb_repeat = true;
						sfb_smooth = true;
						sfb_cid = ids.(i);
						sfb_mpos = {
							scale = Some {
								m_nbits = 22;
								mx = 20 lsl 16;
								my = 20 lsl 16;
							};
							rotate = None;
							trans = {
								m_nbits = 0;
								mx = 0;
								my = 0;
							};
						};
					};
				];
				sws_line_styles = [];
				sws_records = {
					srs_nlbits = 0;
					srs_nfbits = 1;
					srs_records = [
						SRStyleChange {
							scsr_move = None;
							scsr_fs0 = None;
							scsr_fs1 = Some 1;
							scsr_ls = None;
							scsr_new_styles = None;
						};
						SRStraightEdge {
							sser_nbits = nbits;
							sser_line = Some (w * 20) , None;
						};
						SRStraightEdge {
							sser_nbits = nbits;
							sser_line = None , Some (h * 20);
						};
						SRStraightEdge {
							sser_nbits = nbits;
							sser_line = Some (unsigned (-w * 20) nbits), None;
						};
						SRStraightEdge {
							sser_nbits = nbits;
							sser_line = None , Some (unsigned (-h * 20) nbits);
						};
					];
				};
			};
		}
	in
	let shapes = List.mapi loop pics in
	let rec loop i =
		if i = npics then
			[]
		else
			TPlaceObject2 {
				po_depth = 0;
				po_move = (i > 0);
				po_cid = Some (baseid+i);
				po_color = None;
				po_matrix = None;
				po_ratio = None;
				po_inst_name = None;
				po_clip_depth = None;
				po_events = None;
				po_filters = None;
				po_blend = None;
				po_bcache = None;
			} :: TShowFrame :: loop (i+1)
	in
	let tid = ref 0 in
	let make_tag t =
		incr tid;
		{
			tid = - !tid;
			textended = false;
			tdata = t;
		}
	in
	let pics = List.map (fun p -> make_tag p.pdata) pics in
	let shapes = List.map make_tag shapes in
	pics @ shapes @ List.map make_tag [
		TClip {
			c_id = baseid + npics;
			c_frame_count = npics;
			c_tags = List.map make_tag (loop 0);
		};
		TExport [{
			exp_id = baseid + npics;
			exp_name = name;
		}];
	]
