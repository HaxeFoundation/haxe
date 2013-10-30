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

(*
	This data is based on the
		Microsoft Portable Executable and Common Object File Format Specification
	Revision 8.3
*)

type machine_type =
	| TUnknown (* 0 - unmanaged PE files only *)
	| Ti386 (* 0x014c - i386 *)
	| TR3000 (* 0x0162 - R3000 MIPS Little Endian *)
	| TR4000 (* 0x0166 - R4000 MIPS Little Endian *)
	| TR10000 (* 0x0168 - R10000 MIPS Little Endian *)
	| TWCeMipsV2 (* 0x0169 - MIPS Little Endian running MS Windows CE 2 *)
	| TAlpha (* 0x0184 - Alpha AXP *)
	| TSh3 (* 0x01a2 - SH3 Little Endian *)
	| TSh3Dsp (* 0x01a3 SH3DSP Little Endian *)
	| TSh3e (* 0x01a4 SH3E Little Endian *)
	| TSh4 (* 0x01a6 SH4 Little Endian *)
	| TSh5 (* 0x01a8 SH5 *)
	| TArm (* 0x1c0 ARM Little Endian *)
	| TArmN (* 0x1c4 ARMv7 (or higher) Thumb mode only Little Endian *)
	| TArm64 (* 0xaa64 - ARMv8 in 64-bit mode *)
	| TEbc (* 0xebc - EFI byte code *)
	| TThumb (* 0x1c2 ARM processor with Thumb decompressor *)
	| TAm33 (* 0x1d3 AM33 processor *)
	| TPowerPC (* 0x01f0 IBM PowerPC Little Endian *)
	| TPowerPCFP (* 0x01f1 IBM PowerPC with FPU *)
	| TItanium64 (* 0x0200 Intel IA64 (Itanium) *)
	| TMips16 (* 0x0266 MIPS *)
	| TAlpha64 (* 0x0284 Alpha AXP64 *)
	| TMipsFpu (* 0x0366 MIPS with FPU *)
	| TMipsFpu16 (* 0x0466 MIPS16 with FPU *)
	| TTriCore (* 0x0520 Infineon *)
	| TAmd64 (* 0x8664 AMD x64 and Intel E64T *)
	| TM32R (* 0x9041 M32R *)

type coff_prop =
	| RelocsStripped (* 0x1 *)
		(* image file only. Indicates the file contains no base relocations and *)
		(* must be loaded at its preferred base address. Should not be set for MPE files *)
	| ExecutableImage (* 0x2 *)
		(* Indicates that the file is an image file (EXE or DLL). Should be set for MPE files *)
	| LineNumsStripped (* 0x4 *)
		(* COFF line numbers have been removed. This flag should not be set for MPE files *)
		(* because they do not use the debug info embedded in the PE file itself. They are saved on PDB files *)
	| LocalSymsStripped (* 0x8 *)
		(* COFF symbol table entries for local symbols have been removed. It should be set for MPE files *)
	| AgressiveWsTrim (* 0x10 *)
		(* Agressively trim the working set. This flag should not be set for pure-IL MPE files *)
	| LargeAddressAware (* 0x20 *)
		(* Application can handle addresses beyond the 2GB range. This flag should not be set for *)
		(* pure-IL MPE files of versions 1 and 1.1, but can be set for v2.0 files *)
	| BytesReversedLO (* 0x80 *)
		(* Little endian. This flag should not be set for pure-IL MPE files *)
	| Machine32Bit (* 0x100 *)
		(* Machine is based on 32-bit architecture. This flag is usually set by the current *)
		(* versions of code generators producing PE files. V2.0+ can produce 64-bit specific images *)
		(* which don't have this flag set *)
	| DebugStripped (* 0x200 *)
		(* Debug information has been removed from the image file *)
	| RemovableRunFromSwap (* 0x400 *)
		(* If the image file is on removable media, copy and run it from swap file. *)
		(* This flag should no be set for pure-IL MPE files *)
	| NetRunFromSwap (* 0x800 *)
		(* If the image file is on a network, copy and run it from the swap file. *)
		(* This flag should no be set for pure-IL MPE files *)
	| FileSystem (* 0x1000 *)
		(* The image file is a system file (for example, a device driver) *)
		(* This flag should not be set for pure-IL MPE files *)
	| FileDll (* 0x2000 *)
		(* This image file is a DLL rather than an EXE. It cannot be directly run. *)
	| UpSystemOnly (* 0x4000 *)
		(* The image file should be run on an uniprocessor machine only. *)
		(* This flag should not be set for pure-IL MPE files *)
	| BytesReversedHI (* 0x8000 *)
		(* Big endian *)
		(* This flag should not be set for pure-IL MPE files *)

(* represents a virtual address pointer. It's 64-bit on 64-bit executables, and 32-bit otherwise *)
type pointer = int64

(* represents a memory index address on the target architecture. It's 64-bit on 64-bit executables, and 32-bit otherwise *)
type size_t = pointer

(* relative virtual address. *)
(* it's always 32-bit - which means that PE/COFF files are still limited to the 4GB size *)
type rva = int32

(* represents a PE file-bound memory index *)
type size_t_file = int32

(* represents a file offset *)
(* there's no point in defining it as int32, as file seek operations need an int *)
type pointer_file = int

type coff_header = {
	coff_machine : machine_type; (* offset 0 - size 2 . *)
		(* If the managed PE file is intended for various machine types (AnyCPU), it should be Ti386 *)
	coff_nsections : int; (* O2S2 *)
	coff_timestamp : int32; (* O4S4 *)
	coff_symbol_table_pointer : rva; (* O8S4 *)
		(* File pointer of the COFF symbol table. In managed PE files, it is 0 *)
	coff_nsymbols : int; (* O12S4 *)
		(* Number of entries in the COFF symbol table. Should be 0 in managed PE files *)
	coff_optheader_size: int; (* O16S2 *)
		(* Size of the PE header *)
	coff_props : coff_prop list;
}

let coff_default_exe_props = [ ExecutableImage; LineNumsStripped; LocalSymsStripped; (* Machine32Bit; *) ]

let coff_default_dll_props = [ ExecutableImage; LineNumsStripped; LocalSymsStripped; (* Machine32Bit; *) FileDll ]

type pe_magic =
	| P32 (* 0x10b *)
	| PRom (* 0x107 *)
	| P64 (* 0x20b - called PE32+ on the docs *)
		(* allows 64-bit address space while limiting the image size to 2 gb *)

type subsystem =
	| SUnknown (* 0 *)
	| SNative (* 1 *)
		(* Device drivers and native windows processes *)
	| SWGui (* 2 *)
		(* Windows GUI subsystem *)
	| SWCui (* 3 *)
		(* Windows character subsystem *)
	| SPCui (* 7 *)
		(* Posix character subsystem *)
	| SWCeGui (* 9 *)
		(* Windows CE subsystem *)
	| SEfi (* 10 *)
		(* EFI application *)
	| SEfiBoot (* 11 *)
		(* EFI driver with boot services *)
	| SEfiRuntime (* 12 *)
		(* EFI driver with run-time services *)
	| SEfiRom (* 13 *)
		(* EFI ROM Image *)
	| SXbox (* 14 *)

type dll_prop =
	| DDynamicBase (* 0x0040 *)
		(* DLL can be relocated at load time *)
	| DForceIntegrity (* 0x0080 *)
		(* Code integrity checks are enforced *)
	| DNxCompat (* 0x0100 *)
		(* Image is NX compatible *)
	| DNoIsolation (* 0x0200 *)
		(* Isolation-aware, but do not isolate the image *)
	| DNoSeh (* 0x0400 *)
		(* No structured exception handling *)
	| DNoBind (* 0x0800 *)
		(* Do not bind the image *)
	| DWdmDriver (* 0x2000 *)
		(* A WDM driver *)
	| DTerminalServer (* 0x8000 *)
		(* Terminal server aware *)

type directory_type =
	| ExportTable (* .edata *)
		(* contains information about four other tables, which hold data describing *)
		(* unmanaged exports of the PE file. ILAsm and VC++ linker are capable of exposing *)
		(* the managed PE file as unmanaged exports *)
	| ImportTable (* .idata *)
		(* data on unmanaged imports consumed by the PE file. Only the VC++ linker makes *)
		(* use of this table, by marking the imported unmanaged external functions used by *)
		(* the unmanaged native code embedded in the same assembly. Other compilers only *)
		(* contain a single entry - that of the CLR entry function *)
	| ResourceTable (* .rsrc *)
		(* unmanaged resources embedded in the PE file. Managed resources don't use this *)
	| ExceptionTable (* .pdata *)
		(* unmanaged exceptions only *)
	| CertificateTable
		(* points to a table of attribute certificates, used for file authentication *)
		(* the first field of this entry is a file pointer rather than an RVA *)
	| RelocTable (* .reloc *)
		(* relocation table. We need to be aware of it if we use native TLS. *)
		(* only the VC++ linker uses native TLS' *)
	| DebugTable
		(* unmanaged debug data starting address and size. A managed PE file doesn't carry *)
		(* embedded debug data, so this data is either all zero or points to a 30-byte debug dir entry *)
		(* of type 2 (IMAGE_DEBUG_TYPE_CODEVIEW), which in turn points to a CodeView-style header, containing *)
		(* the path to the PDB debug file. *)
	| ArchitectureTable
		(* for i386, Itanium64 or AMD64, this data is set to all zeros *)
	| GlobalPointer
		(* the RVA of the value to be stored in the global pointer register. Size must be 0. *)
		(* if the target architecture (e.g. i386 or AMD64) don't use the concept of a global pointer, *)
		(* it is set to all zeros *)
	| TlsTable (* .tls *)
		(* The thread-local storage data. Only the VC++ linker and IL assembler produce code that use it *)
	| LoadConfigTable
		(* data specific to Windows NT OS *)
	| BoundImportTable
		(* array of bound import descriptors, each of which describes a DLL this image was bound *)
		(* at link-time, along with time stamps of the bindings. Iff they are up-to-date, the OS loader *)
		(* uses these bindings as a "shortcut" for API import *)
	| ImportAddressTable
		(* referenced from the Import Directory table (data directory 1) *)
	| DelayImport
		(* delay-load imports are DLLs described as implicit imports but loaded as explicit imports *)
		(* (via calls to the LoadLibrary API) *)
	| ClrRuntimeHeader (* .cormeta *)
		(* pointer to the clr_runtime_header *)
	| Reserved
		(* must be zero *)
	| Custom of int

let directory_type_info = function
	| ExportTable -> 0, "ExportTable"
	| ImportTable -> 1, "ImportTable"
	| ResourceTable -> 2, "ResourceTable"
	| ExceptionTable -> 3, "ExceptionTable"
	| CertificateTable -> 4, "CertificateTable"
	| RelocTable -> 5, "RelocTable"
	| DebugTable -> 6, "DebugTable"
	| ArchitectureTable -> 7, "ArchTable"
	| GlobalPointer -> 8, "GlobalPointer"
	| TlsTable -> 9, "TlsTable"
	| LoadConfigTable -> 10, "LoadConfigTable"
	| BoundImportTable -> 11, "BuildImportTable"
	| ImportAddressTable -> 12, "ImportAddressTable"
	| DelayImport -> 13, "DelayImport"
	| ClrRuntimeHeader -> 14, "ClrRuntimeHeader"
	| Reserved -> 15, "Reserved"
	| Custom i -> i, "Custom" ^ (string_of_int i)

let directory_type_of_int = function
	| 0 -> ExportTable
	| 1 -> ImportTable
	| 2 -> ResourceTable
	| 3 -> ExceptionTable
	| 4 -> CertificateTable
	| 5 -> RelocTable
	| 6 -> DebugTable
	| 7 -> ArchitectureTable
	| 8 -> GlobalPointer
	| 9 -> TlsTable
	| 10 -> LoadConfigTable
	| 11 -> BoundImportTable
	| 12 -> ImportAddressTable
	| 13 -> DelayImport
	| 14 -> ClrRuntimeHeader
	| 15 -> Reserved
	| i -> Custom i

type section_prop =
	| SNoPad (* 0x8 *)
		(* the section should not be padded to the next boundary. *)
		(* OBSOLETE - replaced by SAlign1Bytes *)
	| SHasCode (* 0x20 *)
		(* the section contains executable code *)
	| SHasIData (* 0x40 *)
		(* contains initialized data *)
	| SHasData (* 0x80 *)
		(* contains uninitialized data *)
	| SHasLinkInfo (* 0x200 *)
		(* contains comments or other information. only valid for object files *)
	| SLinkRemove (* 0x1000 *)
		(* this will not become part of the image. only valid for object files *)
	| SGlobalRel (* 0x8000 *)
		(* contains data referenced through the global pointer (GP) *)
	| SHas16BitMem (* 0x20000 *)
		(* for ARM architecture. The section contains Thumb code *)
	| SAlign1Bytes (* 0x100000 *)
		(* align data on a 1-byte boundary. valid only for object files *)
	| SAlign2Bytes (* 0x200000 *)
	| SAlign4Bytes (* 0x300000 *)
	| SAlign8Bytes (* 0x400000 *)
	| SAlign16Bytes (* 0x500000 *)
	| SAlign32Bytes (* 0x600000 *)
	| SAlign64Bytes (* 0x700000 *)
	| SAlign128Bytes (* 0x800000 *)
	| SAlign256Bytes (* 0x900000 *)
	| SAlign512Bytes (* 0xA00000 *)
	| SAlign1024Bytes (* 0xB00000 *)
	| SAlign2048Bytes (* 0xC00000 *)
	| SAlign4096Bytes (* 0xD00000 *)
	| SAlign8192Bytes (* 0xE00000 *)
	| SHasExtRelocs (* 0x1000000 *)
		(* section contains extended relocations *)
	| SCanDiscard (* 0x02000000 *)
		(* section can be discarded as needed *)
	| SNotCached (* 0x04000000 *)
		(* section cannot be cached *)
	| SNotPaged (* 0x08000000 *)
		(* section is not pageable *)
	| SShared (* 0x10000000 *)
		(* section can be shared in memory *)
	| SExec (* 0x20000000 *)
		(* section can be executed as code *)
	| SRead (* 0x40000000 *)
		(* section can be read *)
	| SWrite (* 0x80000000 *)
		(* section can be written to *)

type pe_section = {
	s_name : string;
		(* an 8-byte, null-padded UTF-8 encoded string *)
	s_vsize : size_t_file;
		(* the total size of the section when loaded into memory. *)
		(* if less than s_rawsize, the section is zero-padded *)
		(* should be set to 0 on object files *)
	s_vaddr : rva;
		(* the RVA of the beginning of the section *)
	s_raw_size : size_t_file;
		(* the size of the initialized data on disk, rounded up to a multiple *)
		(* of the file alignment value. If it's less than s_vsize, it should be *)
		(* zero filled. It may happen that rawsize is greater than vsize. *)
	s_raw_pointer : pointer_file;
		(* the file pointer to the first page of the section within the COFF file *)
		(* on executable images, this must be a multiple of file aignment value. *)
		(* for object files, it should be aligned on a 4byte boundary *)
	s_reloc_pointer : pointer_file;
		(* the file pointer to the beginning of relocation entries for this section *)
		(* this is set to zero for executable images or if there are no relocations *)
	s_line_num_pointer : pointer_file;
		(* the file pointer to the beginning of line-number entries for this section *)
		(* must be 0 : COFF debugging image is deprecated *)
	s_nrelocs : int;
		(* number of relocation entries *)
	s_nline_nums : int;
		(* number of line number entries *)
	s_props : section_prop list;
		(* properties of the section *)
}

(* The size of the PE header is not fixed. It depends on the number of data directories defined in the header *)
(* and is specified in the optheader_size in the COFF header *)
(* object files don't have this; but it's required for image files *)
type pe_header = {
	pe_coff_header : coff_header;
	(* Standard fields *)
	pe_magic : pe_magic;
	pe_major : int;
	pe_minor : int;
	pe_code_size : int;
		(* size of the code section (.text) or the sum of all code sections, *)
		(* if multiple sections exist. The IL assembler always emits a single code section *)
	pe_init_size : int;
	pe_uinit_size : int;
	pe_entry_addr : rva;
		(* RVA of the beginning of the entry point function. For unmanaged DLLs, this can be 0 *)
		(* For managed PE files, this always points to the CLR invocation stub *)
	pe_base_code : rva;
		(* The address that is relative to the image base of the beginning-of-code section *)
		(* when it's loaded into memory *)
	pe_base_data : rva;
		(* The address that is relative to the image base of the beginning-of-data section *)
		(* when it's loaded into memory *)

	(* COFF Windows extension *)
	pe_image_base : pointer;
		(* The preferred address of the first byte of image when loaded into memory. *)
		(* Should be a multiple of 64K *)
	pe_section_alignment : int;
		(* The alignment in bytes of sections when they are loaded into memory *)
		(* It must be greater than or equal to FileAlignment. The default is the page size *)
		(* for the architecture *)
		(* x86 MPE files should have an alignment of 8KB, even though only 4KB would be needed *)
		(* for compatibility with 64-bits *)
	pe_file_alignment : int;
		(* The alignment factor in bytes that is used to align the raw data of sections *)
		(* in the image file. The value should be a POT between 512 and 64K. *)
		(* If secion_alignment is less than architecture's page size, file_alignment must match *)
		(* secion_alignment *)
	pe_major_osver : int;
	pe_minor_osver : int;
	pe_major_imgver : int;
	pe_minor_imgver : int;
	pe_major_subsysver : int;
	pe_minor_subsysver : int;
	pe_image_size : int;
		(* the size of the image in bytes, as the image is loaded into memory *)
		(* must be a multiple of section_alignment *)
	pe_headers_size : int;
		(* the combined size of an MSDOS stub, PE header, and section headers *)
		(* rounded up to a multiple of FileAlignment *)
	pe_checksum : int32;
	pe_subsystem : subsystem;
	pe_dll_props : dll_prop list;
		(* in MPE files of v1.0, always set to 0; In MPE of v1.1 and later, *)
		(* always set to 0x400 (DNoSeh) *)
	pe_stack_reserve : size_t;
		(* the size of the stack to reserve. Only pe_stack_commit is committed *)
	pe_stack_commit : size_t;
		(* the size of the stack to commit *)
	pe_heap_reserve : size_t;
		(* the size of the local heap space to reserve. Only pe_heap_commit is committed *)
	pe_heap_commit : size_t;
		(* the size of the heap to commit *)
	pe_ndata_dir : int;
		(* the number of data-directory entries in the remainder of the optional header *)
		(* should be at least 16. Although is possible to emit more than 16 data directories, *)
		(* all existing managed compilers emit exactly 16 data directories, with the last never *)
		(* used (reserved) *)
	pe_data_dirs : (rva * size_t_file) array;
		(* data directories are RVA's that point to sections on the PE that have special significance *)
		(* see directory_type docs *)

	(* sections *)
	pe_sections : pe_section array;
}

(* raw .idata table *)
(* not used : only here for documentation purposes *)
type idata_table_raw = {
	impr_lookup_table : rva;
		(* the RVA of the lookup table *)
	impr_timestamp : int32;
		(* on bound images, it's set to the timestamp of the DLL *)
	impr_fchain : int32;
		(* the index of the first forwarder reference - which are references *)
		(* that are both imported and exported *)
	impr_name : rva;
		(* the RVA to an ASCII string that contains the name of the DLL *)
	impr_address_table : rva;
		(* RVA of the import address table. The contents are identical to the imp_lookup_table *)
		(* until the image is bound *)
}

(* a symbol lookup can happen either by name, or by ordinal. *)
(* lookup by name happens to be an extra indirection, as the loader *)
(* uses the name to look up the export ordinal anyway. *)
(* Most (if not all) MPE will do a lookup by name, though *)
type symbol_lookup =
	| SName of int * string
	| SOrdinal of int

type idata_table = {
	imp_name : string;
		(* ASCII string that contains the name of the DLL *)
	imp_imports : symbol_lookup list;
}

type clr_flag =
	| FIlOnly (* 0x1 *)
		(* the image file contains IL code only, with no embedded native unmanaged code *)
		(* this can cause some problems on WXP+, because the .reloc section is ignored when this flag is set *)
		(* e.g. if native TLS support is used. In this case the VC++ compiler unsets this flag *)
	| F32BitRequired (* 0x2 *)
		(* the file can be only loaded into a 32-bit process *)
	| FIlLibrary (* 0x4 *)
		(* obsolete *)
	| FSigned (* 0x8 *)
		(* the image file is protected with a strong name signature *)
	| FNativeEntry (* 0x10 *)
		(* the executable's entry point is an unmanaged method. *)
		(* the EntryPointToken / EntryPointRVA field of the CLR header *)
		(* contains the RVA of this native method *)
	| FTrackDebug (* 0x10000 *)
		(* the CLR loader is required to track debug information about the methods. This flag is not used *)

type clr_header = {
	clr_cb : int;
		(* size of header *)
	clr_major : int;
	clr_minor : int;

	(* symbol table and startup information *)
	clr_meta : rva * size_t_file;
	clr_flags : clr_flag list;
	clr_entry_point : rva;
		(* metadata identifier (token) of the entry point for the image file *)
		(* can be 0 for DLL images. This field identifies a method belonging to this module *)
		(* or a module containing the entry point method. This field may contain RVA of the *)
		(* embedded native entry point method, if FNativeEntry flag is set *)

	(* binding information *)
	clr_res : rva * size_t_file;
		(* RVA of managed resources *)
	clr_sig : rva * size_t_file;
		(* RVA of the hash data for this PE file, used by the loader for binding and versioning *)

	(* regular fixup and binding information *)
	clr_codeman : rva * size_t_file;
		(* code manager table - RESERVED and should be 0 *)
	clr_vtable_fix : rva * size_t_file;
		(* RVA of an array of vtable fixups. Only VC++ linker and IL assembler produce data in this array *)
	clr_export_address : rva * size_t_file;
		(* rva of addresses of jump thunks. obsolete and should be set to 0 *)
}

(* unused structure: documentation purposes only *)
type clr_stream_header = {
	str_offset : pointer_file;
		(* the (relative to the start of metadata) offset in the file for this stream *)
	str_size : size_t_file;
		(* the size of the stream in bytes *)
	str_name : string;
		(* name of the stream - a zero-terminated ASCII string no longer than 31 characters (plus 0 terminator) *)
		(* if the stream name is smaller, it can be reduced - but must be padded to the 4-byte boundary *)
}

(* unused structure: documentation purposes only *)
type clr_meta_table = {
	(* storage signature *)
	meta_magic : string;
		(* always BSJB *)
	meta_major : int;
	meta_minor : int;
	(* meta_extra : int; *)
		(* reserved; always 0 *)
	meta_ver : string;
		(* encoded by first passing its length *)

	(* storage header *)
	(* meta_flags : int; *)
		(* reserved; always 0 *)
	meta_nstreams : int;
		(* number of streams *)
	meta_strings_stream : clr_stream_header;
		(* #Strings: a string heap containing the names of metadata items *)
	meta_blob_stream : clr_stream_header;
		(* #Blob: blob heap containing internal metadata binary object, such as default values, signatures, etc *)
	meta_guid_stream : clr_stream_header;
		(* #GUID: a GUID heap *)
	meta_us_stream : clr_stream_header;
		(* #US: user-defined strings *)
	meta_meta_stream : clr_stream_header;
		(* may be either: *)
			(* #~: compressed (optimized) metadata stream *)
			(* #-: uncompressed (unoptimized) metadata stream *)
	meta_streams : clr_stream_header list;
		(* custom streams *)
}
