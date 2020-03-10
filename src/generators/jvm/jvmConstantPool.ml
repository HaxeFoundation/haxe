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

open IO
open IO.BigEndian
open JvmGlobals
open JvmSignature

(* High-level constant pool *)

let utf8jvm (input : string) : bytes =
	let channel = IO.output_bytes () in
	UTF8.iter (fun c ->
		let code = UCharExt.code c in
		match code with
			| b when (b > 0 && b <= 0x7F) ->
			IO.write_byte channel b
			(* includes null byte: *)
			| b when (b <= 0x7FF) ->
			IO.write_byte channel (0xC0 lor ((b lsr  6)          ));
			IO.write_byte channel (0x80 lor ((b       ) land 0x3F))
			| b when (b <= 0xFFFF) ->
			IO.write_byte channel (0xE0 lor ((b lsr 12)          ));
			IO.write_byte channel (0x80 lor ((b lsr  6) land 0x3F));
			IO.write_byte channel (0x80 lor ((b       ) land 0x3F))
			| b ->
			IO.write_byte channel 0xED;
			IO.write_byte channel (0xA0 lor ((b lsr 16) - 1      ));
			IO.write_byte channel (0x80 lor ((b lsr 10) land 0x3F));
			IO.write_byte channel 0xED;
			IO.write_byte channel (0xB0 lor ((b lsr  6) land 0x0F));
			IO.write_byte channel (0x80 lor ((b       ) land 0x3F))
	) input;
	IO.close_out channel
;;

class constant_pool = object(self)
	val pool = DynArray.create ();
	val lut = Hashtbl.create 0;
	val luti = Hashtbl.create 0;
	val mutable next_index = 1;
	val mutable closed = false
	val inner_classes = Hashtbl.create 0

	method add const =
		try
			Hashtbl.find lut const
		with Not_found ->
			assert (not closed);
			let i = next_index in
			next_index <- next_index + 1;
			DynArray.add pool const;
			Hashtbl.add lut const i;
			Hashtbl.add luti i (DynArray.length pool - 1);
			match const with
			| ConstDouble _ | ConstLong _ ->
				next_index <- next_index + 1;
				i
			| _ ->
				i

	method get i =
		DynArray.get pool (Hashtbl.find luti i)

	method private s_type_path (p,s) = match p with [] -> s | _ -> String.concat "/" p ^ "/" ^ s

	method add_type s =
		let offset = self#add (ConstUtf8 s) in
		self#add (ConstClass offset);

	method add_path path =
		let s = self#s_type_path path in
		let offset = self#add_type s in
		if String.contains (snd path) '$' && not (ExtString.String.starts_with s "[") then begin
			let name1,name2 = ExtString.String.split (snd path) "$" in
			Hashtbl.replace inner_classes ((fst path,name1),name2) offset;
		end;
		offset

	method add_string s =
		self#add (ConstUtf8 s)

	method add_const_string s =
		let offset = self#add_string s in
		self#add (ConstString offset)

	method add_name_and_type name jsig field_kind =
		let offset_name = self#add_string name in
		let offset_desc = self#add_string ((if field_kind = FKField then generate_signature else generate_method_signature) false jsig) in
		self#add (ConstNameAndType(offset_name,offset_desc))

	method add_field path name jsig field_kind =
		let offset_class = self#add_path path in
		let offset_info = self#add_name_and_type name jsig field_kind in
		let const = match field_kind with
			| FKField -> ConstFieldref(offset_class,offset_info)
			| FKMethod -> ConstMethodref(offset_class,offset_info)
			| FKInterfaceMethod -> ConstInterfaceMethodref(offset_class,offset_info)
		in
		self#add const

	method get_inner_classes = inner_classes

	method private write_i64 ch i64 =
		write_real_i32 ch (Int64.to_int32 i64);
		write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical i64 32))

	method private write ch =
		write_ui16 ch next_index;
		DynArray.iter (function
			| ConstUtf8 s ->
				write_byte ch 1;
				let b = utf8jvm s in
				write_ui16 ch (Bytes.length b);
				nwrite ch b
			| ConstInt i32 ->
				write_byte ch 3;
				write_real_i32 ch i32;
			| ConstFloat f ->
				write_byte ch 4;
				(match classify_float f with
				| FP_normal | FP_subnormal | FP_zero ->
					write_real_i32 ch (Int32.bits_of_float f)
				| FP_infinite when f > 0.0 ->
					write_real_i32 ch 0x7f800000l
				| FP_infinite ->
					write_real_i32 ch 0xff800000l
				| FP_nan ->
					write_real_i32 ch 0x7f800001l)
			| ConstLong i64 ->
				write_byte ch 5;
				write_i64 ch i64;
			| ConstDouble d ->
				write_byte ch 6;
				write_double ch d
			| ConstClass offset ->
				write_byte ch 7;
				write_ui16 ch offset;
			| ConstString offset ->
				write_byte ch 8;
				write_ui16 ch offset;
			| ConstFieldref (offset1,offset2) ->
				write_byte ch 9;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
			| ConstMethodref (offset1,offset2) ->
				write_byte ch 10;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
			| ConstInterfaceMethodref (offset1,offset2) ->
				write_byte ch 11;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
			| ConstNameAndType (offset1,offset2) ->
				write_byte ch 12;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
			| ConstMethodHandle (i,offset) ->
				write_byte ch 15;
				write_byte ch i;
				write_ui16 ch offset;
			| ConstMethodType offset ->
				write_byte ch 16;
				write_ui16 ch offset;
			| ConstInvokeDynamic (offset1,offset2) ->
				write_byte ch 18;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
		) pool

	method close =
		closed <- true;
		let ch = IO.output_bytes () in
		self#write ch;
		IO.close_out ch
end