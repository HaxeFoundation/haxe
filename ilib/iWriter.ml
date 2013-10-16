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

exception Error_message of string

let error msg = raise (Error_message msg)

type 'a writer_ctx = {
	out : 'a IO.output;
}

let int_of_machine_type t = match t with
	| TUnknown -> 0x0 (* 0 - unmanaged PE files only *)
	| Ti386 -> 0x014c (* 0x014c - i386 *)
	| TR3000 -> 0x0162 (* 0x0162 - R3000 MIPS Little Endian *)
	| TR4000 -> 0x0166 (* 0x0166 - R4000 MIPS Little Endian *)
	| TR10000 -> 0x0168 (* 0x0168 - R10000 MIPS Little Endian *)
	| TWCEMIPSv2 -> 0x0169 (* 0x0169 - MIPS Litlte Endian running MS Windows CE 2 *)
	| TAlpha -> 0x0184 (* 0x0184 - Alpha AXP *)
	| TSH3 -> 0x01a2 (* 0x01a2 - SH3 Little Endian *)
	| TSH3DSP -> 0x01a3 (* 0x01a3 SH3DSP Little Endian *)
	| TSH3E -> 0x01a4 (* 0x01a4 SH3E Little Endian *)
	| TSH4 -> 0x01a6 (* 0x01a6 SH4 Little Endian *)
	| TARM -> 0x01c0 (* 0x1c0 ARM Little Endian *)
	| TThumb -> 0x01c2 (* 0x1c2 ARM processor with Thumb decompressor *)
	| TAM33 -> 0x01d3 (* 0x1d3 AM33 processor *)
	| TPowerPC -> 0x01f0 (* 0x01f0 IBM PowerPC Little Endian *)
	| TPowerPCFP -> 0x01f1 (* 0x01f1 IBM PowerPC with FPU *)
	| TIA64 -> 0x0200 (* 0x0200 Intel IA64 (Itanium( *)
	| TMIPS16 -> 0x0266 (* 0x0266 MIPS *)
	| TALPHA64 -> 0x0284 (* 0x0284 Alpha AXP64 *)
	| TMIPSFPU -> 0x0366 (* 0x0366 MIPS with FPU *)
	| TMIPSFPU16 -> 0x0466 (* 0x0466 MIPS16 with FPU *)
	| TTriCore -> 0x0520 (* 0x0520 Infineon *)
	| TAMD64 -> 0x8664 (* 0x8664 AMD x64 and Intel E64T *)
	| TM32R -> 0x9041 (* 0x9041 M32R *)

let int_of_coff_props props = List.fold_left (fun acc prop ->
		(match prop with
			| RelocsStripped -> 0x1 (* 0x1 *)
			| ExecutableImage -> 0x2 (* 0x2 *)
			| LineNumsStripped -> 0x4 (* 0x4 *)
			| LocalSymsStripped -> 0x8 (* 0x8 *)
			| AgressiveWsTrim -> 0x10 (* 0x10 *)
			| LargeAddressAware -> 0x20 (* 0x20 *)
			| BytesReversedLO -> 0x80 (* 0x80 *)
			| Machine32Bit -> 0x100 (* 0x100 *)
			| DebugStripped -> 0x200 (* 0x200 *)
			| RemovableRunFromSwap -> 0x400 (* 0x400 *)
			| NetRunFromSwap -> 0x800 (* 0x800 *)
			| System -> 0x1000 (* 0x1000 *)
			| Dll -> 0x2000 (* 0x2000 *)
			| UpSystemOnly -> 0x4000 (* 0x4000 *)
			| BytesReversedHI -> 0x8000 (* 0x8000 *)
		) lor acc
	) 0 props
