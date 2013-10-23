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

open PeData;;
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
	| TWCeMipsV2 -> 0x0169 (* 0x0169 - MIPS Litlte Endian running MS Windows CE 2 *)
	| TAlpha -> 0x0184 (* 0x0184 - Alpha AXP *)
	| TSh3 -> 0x01a2 (* 0x01a2 - SH3 Little Endian *)
	| TSh3Dsp -> 0x01a3 (* 0x01a3 SH3DSP Little Endian *)
	| TSh3e -> 0x01a4 (* 0x01a4 SH3E Little Endian *)
	| TSh4 -> 0x01a6 (* 0x01a6 SH4 Little Endian *)
	| TSh5 -> 0x01a8
	| TArm -> 0x01c0 (* 0x1c0 ARM Little Endian *)
	| TArmN -> 0x01c4 (* 0x1c0 ARM Little Endian *)
	| TArm64 -> 0xaa64 (* 0x1c0 ARM Little Endian *)
	| TEbc -> 0xebc
	| TThumb -> 0x01c2 (* 0x1c2 ARM processor with Thumb decompressor *)
	| TAm33 -> 0x01d3 (* 0x1d3 AM33 processor *)
	| TPowerPC -> 0x01f0 (* 0x01f0 IBM PowerPC Little Endian *)
	| TPowerPCFP -> 0x01f1 (* 0x01f1 IBM PowerPC with FPU *)
	| TItanium64 -> 0x0200 (* 0x0200 Intel IA64 (Itanium( *)
	| TMips16 -> 0x0266 (* 0x0266 MIPS *)
	| TAlpha64 -> 0x0284 (* 0x0284 Alpha AXP64 *)
	| TMipsFpu -> 0x0366 (* 0x0366 MIPS with FPU *)
	| TMipsFpu16 -> 0x0466 (* 0x0466 MIPS16 with FPU *)
	| TTriCore -> 0x0520 (* 0x0520 Infineon *)
	| TAmd64 -> 0x8664 (* 0x8664 AMD x64 and Intel E64T *)
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
			| FileSystem -> 0x1000 (* 0x1000 *)
			| FileDll -> 0x2000 (* 0x2000 *)
			| UpSystemOnly -> 0x4000 (* 0x4000 *)
			| BytesReversedHI -> 0x8000 (* 0x8000 *)
		) lor acc
	) 0 props

let int32_of_section_prop props = List.fold_left (fun acc prop ->
		Int32.logor (match prop with
			| SNoPad ->  0x8l (* 0x8 *)
			| SHasCode ->  0x20l (* 0x20 *)
			| SHasIData ->  0x40l (* 0x40 *)
			| SHasData ->  0x80l (* 0x80 *)
			| SHasLinkInfo ->  0x200l (* 0x200 *)
			| SLinkRemove ->  0x1000l (* 0x1000 *)
			| SGlobalRel ->  0x8000l (* 0x8000 *)
			| SHas16BitMem ->  0x20000l (* 0x20000 *)
			| SAlign1Bytes ->  0x100000l (* 0x100000 *)
			| SAlign2Bytes ->  0x200000l (* 0x200000 *)
			| SAlign4Bytes ->  0x300000l (* 0x300000 *)
			| SAlign8Bytes ->  0x400000l (* 0x400000 *)
			| SAlign16Bytes ->  0x500000l (* 0x500000 *)
			| SAlign32Bytes ->  0x600000l (* 0x600000 *)
			| SAlign64Bytes ->  0x700000l (* 0x700000 *)
			| SAlign128Bytes ->  0x800000l (* 0x800000 *)
			| SAlign256Bytes ->  0x900000l (* 0x900000 *)
			| SAlign512Bytes ->  0xA00000l (* 0xA00000 *)
			| SAlign1024Bytes ->  0xB00000l (* 0xB00000 *)
			| SAlign2048Bytes ->  0xC00000l (* 0xC00000 *)
			| SAlign4096Bytes ->  0xD00000l (* 0xD00000 *)
			| SAlign8192Bytes ->  0xE00000l (* 0xE00000 *)
			| SHasExtRelocs ->  0x1000000l (* 0x1000000 *)
			| SCanDiscard ->  0x02000000l (* 0x02000000 *)
			| SNotCached ->  0x04000000l (* 0x04000000 *)
			| SNotPaged ->  0x08000000l (* 0x08000000 *)
			| SShared ->  0x10000000l (* 0x10000000 *)
			| SExec ->  0x20000000l (* 0x20000000 *)
			| SRead ->  0x40000000l (* 0x40000000 *)
			| SWrite ->  0x80000000l (* 0x80000000 *)
		) acc
	) 0l props

let int_of_pe_magic m = match m with
	| P32 -> 0x10b
	| PRom -> 0x107
	| P64 -> 0x20b

let int_of_subsystem s = match s with
	|  SUnknown -> 0 (* 0 *)
	|  SNative -> 1 (* 1 *)
	|  SWGui -> 2 (* 2 *)
	|  SWCui -> 3 (* 3 *)
	|  SPCui -> 7 (* 7 *)
	|  SWCeGui -> 9 (* 9 *)
	|  SEfi -> 10 (* 10 *)
	|  SEfiBoot -> 11 (* 11 *)
	|  SEfiRuntime -> 12 (* 12 *)
	|  SEfiRom -> 13 (* 13 *)
	|  SXbox -> 14 (* 14 *)

let int_of_dll_props props = List.fold_left (fun acc prop ->
		(match prop with
		| DDynamicBase -> 0x0040 (* 0x0040 *)
		| DForceIntegrity -> 0x0080 (* 0x0080 *)
		| DNxCompat -> 0x0100 (* 0x0100 *)
		| DNoIsolation -> 0x0200 (* 0x0200 *)
		| DNoSeh -> 0x0400 (* 0x0400 *)
		| DNoBind -> 0x0800 (* 0x0800 *)
		| DWdmDriver -> 0x2000 (* 0x2000 *)
		| DTerminalServer -> 0x8000 (* 0x8000 *)
		) lor acc
	) 0 props

let int_of_clr_flags props = List.fold_left (fun acc prop ->
		(match prop with
		| FIlOnly ->  0x1  (* 0x1 *)
		| F32BitRequired ->  0x2  (* 0x2 *)
		| FIlLibrary ->  0x4  (* 0x4 *)
		| FSigned ->  0x8  (* 0x8 *)
		| FNativeEntry ->  0x10  (* 0x10 *)
		| FTrackDebug ->  0x10000  (* 0x10000 *)
		) lor acc
	) 0 props
