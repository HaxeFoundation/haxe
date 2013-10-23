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
open Printf;;

let machine_type_s m = match m with
	| TUnknown -> "TUnknown"
	| Ti386 -> "Ti386"
	| TR3000 -> "TR3000"
	| TR4000 -> "TR4000"
	| TR10000 -> "TR10000"
	| TWCeMipsV2 -> "TWCeMipsV2"
	| TAlpha -> "TAlpha"
	| TSh3 -> "TSh3"
	| TSh3Dsp -> "TSh3Dsp"
	| TSh3e -> "TSh3e"
	| TSh4 -> "TSh4"
	| TSh5 -> "TSh5"
	| TArm -> "TArm"
	| TArmN -> "TArmN"
	| TArm64 -> "TArm64"
	| TEbc -> "TEbc"
	| TThumb -> "TThumb"
	| TAm33 -> "TAm33"
	| TPowerPC -> "TPowerPC"
	| TPowerPCFP -> "TPowerPCFP"
	| TItanium64 -> "TItanium64"
	| TMips16 -> "TMips16"
	| TAlpha64 -> "TAlpha64"
	| TMipsFpu -> "TMipsFpu"
	| TMipsFpu16 -> "TMipsFpu16"
	| TTriCore -> "TTriCore"
	| TAmd64 -> "TAmd64"
	| TM32R -> "TM32R"

let coff_prop_s p = match p with
	| RelocsStripped -> "RelocsStripped"
	| ExecutableImage -> "ExecutableImage"
	| LineNumsStripped -> "LineNumsStripped"
	| LocalSymsStripped -> "LocalSymsStripped"
	| AgressiveWsTrim -> "AgressiveWsTrim"
	| LargeAddressAware -> "LargeAddressAware"
	| BytesReversedLO -> "BytesReversedLO"
	| Machine32Bit -> "Machine32Bit"
	| DebugStripped -> "DebugStripped"
	| RemovableRunFromSwap -> "RemovableRunFromSwap"
	| NetRunFromSwap -> "NetRunFromSwap"
	| FileSystem -> "FileSystem"
	| FileDll -> "FileDll"
	| UpSystemOnly -> "UpSystemOnly"
	| BytesReversedHI -> "BytesReversedHI"

let coff_header_s h =
	sprintf "#COFF_HEADER\n\tmachine: %s\n\tnsections: %d\n\ttimestamp: %ld\n\tsymbol_tbl_pointer: %ld\n\tnsymbols: %d\n\toptheader_size: %x\n\tprops: [%s]\n"
		(machine_type_s h.coff_machine)
		h.coff_nsections
		h.coff_timestamp
		h.coff_symbol_table_pointer
		h.coff_nsymbols
		h.coff_optheader_size
		(String.concat ", " (List.map coff_prop_s h.coff_props))

let pe_magic_s = function
	| P32 -> "P32"
	| PRom -> "PRom"
	| P64 -> "P64"

let subsystem_s = function
	| SUnknown -> "SUnknown" (* 0 *)
	| SNative -> "SNative" (* 1 *)
	| SWGui -> "SWGui" (* 2 *)
	| SWCui -> "SWCui" (* 3 *)
	| SPCui -> "SPCui" (* 7 *)
	| SWCeGui -> "SWCeGui" (* 9 *)
	| SEfi -> "SEfi" (* 10 *)
	| SEfiBoot -> "SEfiBoot" (* 11 *)
	| SEfiRuntime -> "SEfiRuntime" (* 12 *)
	| SEfiRom -> "SEfiRom" (* 13 *)
	| SXbox -> "SXbox" (* 14 *)

let dll_prop_s = function
	| DDynamicBase -> "DDynamicBase" (* 0x0040 *)
	| DForceIntegrity -> "DForceIntegrity" (* 0x0080 *)
	| DNxCompat -> "DNxCompat" (* 0x0100 *)
	| DNoIsolation -> "DNoIsolation" (* 0x0200 *)
	| DNoSeh -> "DNoSeh" (* 0x0400 *)
	| DNoBind -> "DNoBind" (* 0x0800 *)
	| DWdmDriver -> "DWdmDriver" (* 0x2000 *)
	| DTerminalServer -> "DTerminalServer" (* 0x8000 *)

let section_prop_s = function
	| SNoPad -> "SNoPad"
	| SHasCode -> "SHasCode"
	| SHasIData -> "SHasIData"
	| SHasData -> "SHasData"
	| SHasLinkInfo -> "SHasLinkInfo"
	| SLinkRemove -> "SLinkRemove"
	| SGlobalRel -> "SGlobalRel"
	| SHas16BitMem -> "SHas16BitMem"
	| SAlign1Bytes -> "SAlign1Bytes"
	| SAlign2Bytes -> "SAlign2Bytes"
	| SAlign4Bytes -> "SAlign4Bytes"
	| SAlign8Bytes -> "SAlign8Bytes"
	| SAlign16Bytes -> "SAlign16Bytes"
	| SAlign32Bytes -> "SAlign32Bytes"
	| SAlign64Bytes -> "SAlign64Bytes"
	| SAlign128Bytes -> "SAlign128Bytes"
	| SAlign256Bytes -> "SAlign256Bytes"
	| SAlign512Bytes -> "SAlign512Bytes"
	| SAlign1024Bytes -> "SAlign1024Bytes"
	| SAlign2048Bytes -> "SAlign2048Bytes"
	| SAlign4096Bytes -> "SAlign4096Bytes"
	| SAlign8192Bytes -> "SAlign8192Bytes"
	| SHasExtRelocs -> "SHasExtRelocs"
	| SCanDiscard -> "SCanDiscard"
	| SNotCached -> "SNotCached"
	| SNotPaged -> "SNotPaged"
	| SShared -> "SShared"
	| SExec -> "SExec"
	| SRead -> "SRead"
	| SWrite -> "SWrite"

let pe_section_s s =
	Printf.sprintf "\t%s :\n\t\trva: %lx\n\t\traw size: %lx\n\t\tprops: [%s]"
		s.s_name
		s.s_vaddr
		s.s_raw_size
		(String.concat ", " (List.map section_prop_s s.s_props))

let data_dirs_s a =
	let lst = Array.to_list (Array.mapi (fun i (r,l) ->
		let _,s = directory_type_info (directory_type_of_int i) in
		Printf.sprintf "%s: %lx (%lx)" s r l
	) a) in
	String.concat "\n\t\t" lst

let pe_header_s h =
	sprintf "#PE_HEADER\n\tmagic: %s\n\tmajor.minor %d.%d\n\tsubsystem: %s\n\tdll props: [%s]\n\tndata_dir: %i\n\t\t%s\n#SECTIONS\n%s"
		(pe_magic_s h.pe_magic)
		h.pe_major h.pe_minor
		(subsystem_s h.pe_subsystem)
		(String.concat ", " (List.map dll_prop_s h.pe_dll_props))
		h.pe_ndata_dir
		(data_dirs_s h.pe_data_dirs)
		(String.concat "\n" (List.map pe_section_s (Array.to_list h.pe_sections)))

let symbol_lookup_s = function
	| SName (hint,s) -> "SName(" ^ string_of_int hint ^ ", " ^ s ^ ")"
	| SOrdinal i -> "SOrdinal(" ^ string_of_int i ^ ")"

let idata_table_s t =
	sprintf "#IMPORT %s:\n\t%s"
		t.imp_name
		(String.concat "\n\t" (List.map symbol_lookup_s t.imp_imports))

let clr_flag_s = function
	| FIlOnly -> "FIlOnly" (* 0x1 *)
	| F32BitRequired -> "F32BitRequired" (* 0x2 *)
	| FIlLibrary -> "FIlLibrary" (* 0x4 *)
	| FSigned -> "FSigned" (* 0x8 *)
	| FNativeEntry -> "FNativeEntry" (* 0x10 *)
	| FTrackDebug -> "FTrackDebug" (* 0x10000 *)

let clr_header_s h =
	sprintf "#CLR v%d.%d\n\tflags: %s"
		h.clr_major
		h.clr_minor
		(String.concat ", " (List.map clr_flag_s h.clr_flags))
