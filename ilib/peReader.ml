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

type reader_ctx = {
	ch : Pervasives.in_channel;
	i : IO.input;
	verbose : bool;
}

type ctx = {
	r : reader_ctx;
	pe_header : pe_header;
	read_word : IO.input -> pointer;
}

let error msg = raise (Error_message msg)

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
	| 0x0169 -> TWCeMipsV2 (* 0x0169 - MIPS Litlte Endian running MS Windows CE 2 *)
	| 0x0184 -> TAlpha (* 0x0184 - Alpha AXP *)
	| 0x01a2 -> TSh3 (* 0x01a2 - SH3 Little Endian *)
	| 0x01a3 -> TSh3Dsp (* 0x01a3 SH3DSP Little Endian *)
	| 0x01a4 -> TSh3e (* 0x01a4 SH3E Little Endian *)
	| 0x01a6 -> TSh4 (* 0x01a6 SH4 Little Endian *)
	| 0x01a8 -> TSh5
	| 0x01c0 -> TArm (* 0x1c0 ARM Little Endian *)
	| 0x01c2 -> TThumb (* 0x1c2 ARM processor with Thumb decompressor *)
	| 0x01c4 -> TArmN (* 0x1c0 ARM Little Endian *)
	| 0xaa64 -> TArm64
	| 0xebc -> TEbc
	| 0x01d3 -> TAm33 (* 0x1d3 AM33 processor *)
	| 0x01f0 -> TPowerPC (* 0x01f0 IBM PowerPC Little Endian *)
	| 0x01f1 -> TPowerPCFP (* 0x01f1 IBM PowerPC with FPU *)
	| 0x0200 -> TItanium64 (* 0x0200 Intel IA64 (Itanium( *)
	| 0x0266 -> TMips16 (* 0x0266 MIPS *)
	| 0x0284 -> TAlpha64 (* 0x0284 Alpha AXP64 *)
	| 0x0366 -> TMipsFpu (* 0x0366 MIPS with FPU *)
	| 0x0466 -> TMipsFpu16 (* 0x0466 MIPS16 with FPU *)
	| 0x0520 -> TTriCore (* 0x0520 Infineon *)
	| 0x8664 -> TAmd64 (* 0x8664 AMD x64 and Intel E64T *)
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

let section_props_of_int32 props = List.fold_left (fun acc i ->
	if (Int32.logand props i) = i then (match i with
		| 0x8l -> SNoPad
		| 0x20l -> SHasCode
		| 0x40l -> SHasIData
		| 0x80l -> SHasData
		| 0x200l -> SHasLinkInfo
		| 0x1000l -> SLinkRemove
		| 0x8000l -> SGlobalRel
		| 0x20000l -> SHas16BitMem
		| 0x100000l -> SAlign1Bytes
		| 0x200000l -> SAlign2Bytes
		| 0x300000l -> SAlign4Bytes
		| 0x400000l -> SAlign8Bytes
		| 0x500000l -> SAlign16Bytes
		| 0x600000l -> SAlign32Bytes
		| 0x700000l -> SAlign64Bytes
		| 0x800000l -> SAlign128Bytes
		| 0x900000l -> SAlign256Bytes
		| 0xA00000l -> SAlign512Bytes
		| 0xB00000l -> SAlign1024Bytes
		| 0xC00000l -> SAlign2048Bytes
		| 0xD00000l -> SAlign4096Bytes
		| 0xE00000l -> SAlign8192Bytes
		| 0x1000000l -> SHasExtRelocs
		| 0x02000000l -> SCanDiscard
		| 0x04000000l -> SNotCached
		| 0x08000000l -> SNotPaged
		| 0x10000000l -> SShared
		| 0x20000000l -> SExec
		| 0x40000000l -> SRead
		| 0x80000000l -> SWrite
		| _ -> assert false) :: acc
	else
		acc) [] [ 0x8l;  0x20l;  0x40l;  0x80l;  0x200l;  0x1000l;  0x8000l;  0x20000l;  0x100000l;  0x200000l;  0x300000l;  0x400000l;  0x500000l;  0x600000l;  0x700000l;  0x800000l;  0x900000l;  0xA00000l;  0xB00000l;  0xC00000l;  0xD00000l;  0xE00000l;  0x1000000l;  0x02000000l;  0x04000000l;  0x08000000l;  0x10000000l;  0x20000000l;  0x40000000l;  0x80000000l; ]

let subsystem_of_int i = match i with
	|  0 -> SUnknown (* 0 *)
	|  1 -> SNative (* 1 *)
	|  2 -> SWGui (* 2 *)
	|  3 -> SWCui (* 3 *)
	|  7 -> SPCui (* 7 *)
	|  9 -> SWCeGui (* 9 *)
	|  10 -> SEfi (* 10 *)
	|  11 -> SEfiBoot (* 11 *)
	|  12 -> SEfiRuntime (* 12 *)
	|  13 -> SEfiRom (* 13 *)
	|  14 -> SXbox (* 14 *)
	| _ -> error ("Unknown subsystem " ^ string_of_int i)

let dll_props_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x0040  -> DDynamicBase (* 0x0040 *)
		| 0x0080  -> DForceIntegrity (* 0x0080 *)
		| 0x0100  -> DNxCompat (* 0x0100 *)
		| 0x0200  -> DNoIsolation (* 0x0200 *)
		| 0x0400  -> DNoSeh (* 0x0400 *)
		| 0x0800  -> DNoBind (* 0x0800 *)
		| 0x2000  -> DWdmDriver (* 0x2000 *)
		| 0x8000  -> DTerminalServer (* 0x8000 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x40;0x80;0x100;0x200;0x400;0x800;0x2000;0x8000]

let pe_magic_of_int i = match i with
	| 0x10b -> P32
	| 0x107 -> PRom
	| 0x20b -> P64
	| _ -> error ("Unknown PE magic number: " ^ string_of_int i)

let clr_flags_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x1 -> FIlOnly (* 0x1 *)
		| 0x2 -> F32BitRequired (* 0x2 *)
		| 0x4 -> FIlLibrary (* 0x4 *)
		| 0x8 -> FSigned (* 0x8 *)
		| 0x10 -> FNativeEntry (* 0x10 *)
		| 0x10000 -> FTrackDebug (* 0x10000 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x1;0x2;0x4;0x8;0x10;0x10000]

let get_dir dir ctx =
	let idx,name = directory_type_info dir in
	try
		ctx.pe_header.pe_data_dirs.(idx)
	with
		| Invalid_argument _ ->
			error (Printf.sprintf "The directory '%s' of index '%i' is required but is missing on this file" name idx)

let read_rva = read_real_i32

let read_word is64 i =
	if is64 then read_i64 i else Int64.logand (Int64.of_int32 (read_real_i32 i)) 0xFFFFFFFFL

let read_coff_header i =
	let machine = machine_type_of_int (read_ui16 i) in
	let nsections = read_ui16 i in
	let stamp = read_real_i32 i in
	let symbol_table_pointer = read_rva i in
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

let read_pe_header r header =
	let i = r.i in
	let sections_offset = (pos r) + header.coff_optheader_size in
	let magic = pe_magic_of_int (read_ui16 i) in
	let major = read_byte i in
	let minor = read_byte i in
	let code_size = read_i32 i in
	let init_size = read_i32 i in
	let uinit_size = read_i32 i in
	let entry_addr = read_rva i in
	let base_code = read_rva i in
	let base_data, read_word = match magic with
	| P32 | PRom ->
		read_rva i, read_word false
	| P64 ->
		Int32.zero, read_word true
	in

	(* COFF Windows extension *)
	let image_base = read_word i in
	let section_alignment = read_i32 i in
	let file_alignment = read_i32 i in
	let major_osver = read_ui16 i in
	let minor_osver = read_ui16 i in
	let major_imgver = read_ui16 i in
	let minor_imgver = read_ui16 i in
	let major_subsysver = read_ui16 i in
	let minor_subsysver = read_ui16 i in
	ignore (read_i32 i); (* reserved *)
	let image_size = read_i32 i in
	let headers_size = read_i32 i in
	let checksum = read_real_i32 i in
	let subsystem = subsystem_of_int (read_ui16 i) in
	let dll_props = dll_props_of_int (read_ui16 i) in
	let stack_reserve = read_word i in
	let stack_commit = read_word i in
	let heap_reserve = read_word i in
	let heap_commit = read_word i in
	ignore (read_i32 i); (* reserved *)
	let ndata_dir = read_i32 i in
	let data_dirs = Array.init ndata_dir (fun n ->
		let addr = read_rva i in
		let size = read_rva i in
		addr,size)
	in
	(* sections *)
	let nsections = header.coff_nsections in
	seek r sections_offset;
	let sections = Array.init nsections (fun n ->
		let name = nread i 8 in
		let name = try
			let index = String.index name '\x00' in
			String.sub name 0 index
		with | Not_found ->
				name
		in
		(*TODO check for slash names *)
		let vsize = read_rva i in
		let vaddr = read_rva i in
		let raw_size = read_rva i in
		let raw_pointer = read_i32 i in
		let reloc_pointer = read_i32 i in
		let line_num_pointer = read_i32 i in
		let nrelocs = read_ui16 i in
		let nline_nums = read_ui16 i in
		let props = section_props_of_int32 (read_rva i) in
		{
			s_name = name;
			s_vsize =vsize;
			s_vaddr =vaddr;
			s_raw_size =raw_size;
			s_raw_pointer =raw_pointer;
			s_reloc_pointer =reloc_pointer;
			s_line_num_pointer =line_num_pointer;
			s_nrelocs =nrelocs;
			s_nline_nums =nline_nums;
			s_props =props;
		}
	) in
	{
		pe_coff_header = header;
		pe_magic = magic;
		pe_major = major;
		pe_minor = minor;
		pe_code_size = code_size;
		pe_init_size = init_size;
		pe_uinit_size = uinit_size;
		pe_entry_addr = entry_addr;
		pe_base_code = base_code;
		pe_base_data = base_data;
		pe_image_base = image_base;
		pe_section_alignment = section_alignment;
		pe_file_alignment = file_alignment;
		pe_major_osver = major_osver;
		pe_minor_osver = minor_osver;
		pe_major_imgver = major_imgver;
		pe_minor_imgver = minor_imgver;
		pe_major_subsysver = major_subsysver;
		pe_minor_subsysver = minor_subsysver;
		pe_image_size = image_size;
		pe_headers_size = headers_size;
		pe_checksum = checksum;
		pe_subsystem = subsystem;
		pe_dll_props = dll_props;
		pe_stack_reserve = stack_reserve;
		pe_stack_commit = stack_commit;
		pe_heap_reserve = heap_reserve;
		pe_heap_commit = heap_commit;
		pe_ndata_dir = ndata_dir;
		pe_data_dirs = data_dirs;
		pe_sections = sections;
	}

let create_r ch props =
	let verbose = PMap.mem "IL_VERBOSE" props in
	let i = IO.input_channel ch in
	{
		ch = ch;
		i = i;
		verbose = verbose;
	}

(* converts an RVA into a file offset. *)
let convert_rva ctx rva =
	let sections = ctx.pe_header.pe_sections in
	let nsections = Array.length sections in
	let sec =
		(* linear search. TODO maybe binary search for many sections? *)
		let rec loop n =
			if n >= nsections then error (Printf.sprintf "The RVA %lx is outside sections bounds!" rva);
			let sec = sections.(n) in
			if rva >= sec.s_vaddr && (rva < (Int32.add sec.s_vaddr sec.s_raw_size)) then
				sec
			else
				loop (n+1)
		in
		loop 0
	in
	let diff = Int32.to_int (Int32.sub rva sec.s_vaddr) in
	sec.s_raw_pointer + diff

let seek_rva ctx rva = seek ctx.r (convert_rva ctx rva)

let read_cstring i =
	let ret = Buffer.create 8 in
	let rec loop () =
		let chr = read i in
		if chr = '\x00' then
			Buffer.contents ret
		else begin
			Buffer.add_char ret chr;
			loop()
		end
	in
	loop()

(* reads import data *)
let read_idata ctx = match get_dir ImportTable ctx with
	| 0l,_ | _,0l ->
		[]
	| rva,size ->
		seek_rva ctx rva;
		let i = ctx.r.i in
		let rec loop acc =
			let lookup_table = read_rva i in
			if lookup_table = Int32.zero then
				acc
			else begin
				let timestamp = read_real_i32 i in
				let fchain = read_real_i32 i in
				let name_rva = read_rva i in
				let addr_table = read_rva i in
				ignore addr_table; ignore fchain; ignore timestamp;
				loop ((lookup_table,name_rva) :: acc)
			end
		in
		let tables = loop [] in
		List.rev_map (function (lookup_table,name_rva) ->
			seek_rva ctx lookup_table;
			let is_64 = ctx.pe_header.pe_magic = P64 in
			let imports_data = if not is_64 then
				let rec loop acc =
					let flags = read_real_i32 i in
					if flags = Int32.zero then
						acc
					else begin
						let is_ordinal = Int32.logand flags 0x80000000l = 0x80000000l in
						loop ( (is_ordinal, if is_ordinal then Int32.logand flags 0xFFFFl else Int32.logand flags 0x7FFFFFFFl) :: acc )
					end
				in
				loop []
			else
				let rec loop acc =
					let flags = read_i64 i in
					if flags = Int64.zero then
						acc
					else begin
						let is_ordinal = Int64.logand flags 0x8000000000000000L = 0x8000000000000000L in
						loop ( (is_ordinal, Int64.to_int32 (if is_ordinal then Int64.logand flags 0xFFFFL else Int64.logand flags 0x7FFFFFFFL)) :: acc )
					end
				in
				loop []
			in
			let imports = List.rev_map (function
				| true, ord ->
					SOrdinal (Int32.to_int ord)
				| false, rva ->
					seek_rva ctx rva;
					let hint = read_ui16 i in
					SName (hint, read_cstring i)
			) imports_data in
			seek_rva ctx name_rva;
			let name = read_cstring i in
			{
				imp_name = name;
				imp_imports = imports;
			}
		) tables

let has_clr_header ctx = match get_dir ClrRuntimeHeader ctx with
	| 0l,_ | _,0l ->
		false
	| _ ->
		true

let read_clr_header ctx = match get_dir ClrRuntimeHeader ctx with
	| 0l,_ | _,0l ->
		error "This PE file does not have managed content"
	| rva,size ->
		seek_rva ctx rva;
		let i = ctx.r.i in
		let cb = read_i32 i in
		let major = read_ui16 i in
		let minor = read_ui16 i in
		let read_tbl i =
			let rva = read_rva i in
			let size = read_real_i32 i in
			rva,size
		in
		let meta = read_tbl i in
		let corflags = clr_flags_of_int (read_i32 i) in
		let entry_point = read_rva i in
		let res = read_tbl i in
		let clrsig = read_tbl i in
		let codeman = read_tbl i in
		let vtable_fix = read_tbl i in
		let export_addr = read_tbl i in
		{
			clr_cb = cb;
			clr_major = major;
			clr_minor = minor;
			clr_meta = meta;
			clr_flags = corflags;
			clr_entry_point = entry_point;
			clr_res = res;
			clr_sig = clrsig;
			clr_codeman = codeman;
			clr_vtable_fix = vtable_fix;
			clr_export_address = export_addr;
		}

let read r =
	let i = r.i in
	if read i <> 'M' || read i <> 'Z' then
		error "MZ magic header not found: Is the target file really a PE?";
	seek r 0x3c;
	let pe_sig_offset = read_i32 i in
	seek r pe_sig_offset;
	if really_nread i 4 <> "PE\x00\x00" then
		error "Invalid PE header signature: PE expected";
	let header = read_coff_header i in
	let pe_header = read_pe_header r header in
	{
		r = r;
		pe_header = pe_header;
		read_word = read_word (pe_header.pe_magic = P64);
	}
