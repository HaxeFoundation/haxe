(*
 *  This file is part of ilLib
 *  Copyright (c)2004-2013 Haxe Foundation
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

open IData;;
open IO;;
open ExtString;;
open ExtList;;

exception Error_message of string * string

type reader_ctx = {
	fname : string;
	ch : Pervasives.in_channel;
	i : IO.input;
	verbose : bool;
}

let error r msg = raise (Error_message (msg,r.fname))

let seek r pos =
	seek_in r.ch pos

let pos r =
	Pervasives.pos_in r.ch

let info r msg =
	if r.verbose then
		print_endline (msg())

let machine_type_of_int i = match i with
	| 0x0 -> TUnknown (* 0 - unmanaged PE files only *)
	| 0x014c -> Ti386 (* 0x014c - i386 *)
	| 0x0162 -> TR3000 (* 0x0162 - R3000 MIPS Little Endian *)
	| 0x0166 -> TR4000 (* 0x0166 - R4000 MIPS Little Endian *)
	| 0x0168 -> TR10000 (* 0x0168 - R10000 MIPS Little Endian *)
	| 0x0169 -> TWCEMIPSv2 (* 0x0169 - MIPS Litlte Endian running MS Windows CE 2 *)
	| 0x0184 -> TAlpha (* 0x0184 - Alpha AXP *)
	| 0x01a2 -> TSH3 (* 0x01a2 - SH3 Little Endian *)
	| 0x01a3 -> TSH3DSP (* 0x01a3 SH3DSP Little Endian *)
	| 0x01a4 -> TSH3E (* 0x01a4 SH3E Little Endian *)
	| 0x01a6 -> TSH4 (* 0x01a6 SH4 Little Endian *)
	| 0x01c0 -> TARM (* 0x1c0 ARM Little Endian *)
	| 0x01c2 -> TThumb (* 0x1c2 ARM processor with Thumb decompressor *)
	| 0x01c4 -> TARMN (* 0x1c0 ARM Little Endian *)
	| 0xaa64 -> TARM64
	| 0x01d3 -> TAM33 (* 0x1d3 AM33 processor *)
	| 0x01f0 -> TPowerPC (* 0x01f0 IBM PowerPC Little Endian *)
	| 0x01f1 -> TPowerPCFP (* 0x01f1 IBM PowerPC with FPU *)
	| 0x0200 -> TIA64 (* 0x0200 Intel IA64 (Itanium( *)
	| 0x0266 -> TMIPS16 (* 0x0266 MIPS *)
	| 0x0284 -> TALPHA64 (* 0x0284 Alpha AXP64 *)
	| 0x0366 -> TMIPSFPU (* 0x0366 MIPS with FPU *)
	| 0x0466 -> TMIPSFPU16 (* 0x0466 MIPS16 with FPU *)
	| 0x0520 -> TTriCore (* 0x0520 Infineon *)
	| 0x8664 -> TAMD64 (* 0x8664 AMD x64 and Intel E64T *)
	| 0x9041 -> TM32R (* 0x9041 M32R *)
	| _ -> assert false

let coff_props_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x1 -> RelocsStripped (* 0x1 *)
		| 0x2 -> ExecutableImage (* 0x2 *)
		| 0x4 -> LineNumsStripped (* 0x4 *)
		| 0x8 -> LocalSymsStripped (* 0x8 *)
		| 0x10 -> AgressiveWsTrim (* 0x10 *)
		| 0x20 -> LargeAddressAware (* 0x20 *)
		| 0x80 -> BytesReversedLO (* 0x80 *)
		| 0x100 -> Machine32Bit (* 0x100 *)
		| 0x200 -> DebugStripped (* 0x200 *)
		| 0x400 -> RemovableRunFromSwap (* 0x400 *)
		| 0x800 -> NetRunFromSwap (* 0x800 *)
		| 0x1000 -> FileSystem (* 0x1000 *)
		| 0x2000 -> FileDll (* 0x2000 *)
		| 0x4000 -> UpSystemOnly (* 0x4000 *)
		| 0x8000 -> BytesReversedHI (* 0x8000 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x1;0x2;0x4;0x8;0x10;0x20;0x80;0x100;0x200;0x400;0x800;0x1000;0x2000;0x4000;0x8000]

let read_coff_header i =
	let machine = machine_type_of_int (read_ui16 i) in
	let nsections = read_ui16 i in
	let stamp = read_real_i32 i in
	let symbol_table_pointer = read_i32 i in
	let nsymbols = read_i32 i in
	let optheader_size = read_ui16 i in
	let props = read_ui16 i in
	let props = coff_props_of_int (props) in
	{
		coff_machine = machine;
		coff_nsections = nsections;
		coff_timestamp = stamp;
		coff_symbol_table_pointer = symbol_table_pointer;
		coff_nsymbols = nsymbols;
		coff_optheader_size = optheader_size;
		coff_props = props;
	}

let read name ch =
	let i = IO.input_channel ch in
	let r = {
		fname = name;
		ch = ch;
		i = i;
		verbose = true;
	} in
	if read i <> 'M' || read i <> 'Z' then
		error r "MZ magic header not found: Is the target file really a PE?";
	seek r 0x3c;
	let pe_sig_offset = read_i32 i in
	seek r pe_sig_offset;
	if really_nread i 4 <> "PE\x00\x00" then
		error r "Invalid PE header signature: PE expected";
	let header = read_coff_header i in
	info r (fun () -> coff_header_s header);

