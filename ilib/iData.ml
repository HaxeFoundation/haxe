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
type pointer_file = int32

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
	s_rawsize : size_t_file;
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
	pe_codesize : int;
		(* size of the code section (.text) or the sum of all code sections, *)
		(* if multiple sections exist. The IL assembler always emits a single code section *)
	pe_initsize : int;
	pe_uinitsize : int;
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

type ipath = (string list) * string

(*
type jversion = int * int (* minor + major *)

(** unqualified names cannot have the characters '.', ';', '[' or '/' *)
type unqualified_name = string

type jwildcard =
  | WExtends (* + *)
  | WSuper (* -  *)
  | WNone

type jtype_argument =
  | TType of jwildcard * jsignature
  | TAny (* * *)

and jsignature =
  | TByte (* B *)
  | TChar (* C *)
  | TDouble (* D *)
  | TFloat (* F *)
  | TInt (* I *)
  | TLong (* J *)
  | TShort (* S *)
  | TBool (* Z *)
  | TObject of jpath * jtype_argument list (* L Classname *)
  | TObjectInner of (string list) * (string * jtype_argument list) list (* L Classname ClassTypeSignatureSuffix *)
  | TArray of jsignature * int option (* [ *)
  | TMethod of jmethod_signature (* ( *)
  | TTypeParameter of string (* T *)

(* ( jsignature list ) ReturnDescriptor (| V | jsignature) *)
and jmethod_signature = jsignature list * jsignature option

(* InvokeDynamic-specific: Method handle *)
type reference_type =
  | RGetField (* constant must be ConstField *)
  | RGetStatic (* constant must be ConstField *)
  | RPutField (* constant must be ConstField *)
  | RPutStatic (* constant must be ConstField *)
  | RInvokeVirtual (* constant must be Method *)
  | RInvokeStatic (* constant must be Method *)
  | RInvokeSpecial (* constant must be Method *)
  | RNewInvokeSpecial (* constant must be Method with name <init> *)
  | RInvokeInterface (* constant must be InterfaceMethod *)

(* TODO *)
type bootstrap_method = int

type jconstant =
  (** references a class or an interface - jpath must be encoded as StringUtf8 *)
  | ConstClass of jpath (* tag = 7 *)
  (** field reference *)
  | ConstField of (jpath * unqualified_name * jsignature) (* tag = 9 *)
  (** method reference; string can be special "<init>" and "<clinit>" values *)
  | ConstMethod of (jpath * unqualified_name * jmethod_signature) (* tag = 10 *)
  (** interface method reference *)
  | ConstInterfaceMethod of (jpath * unqualified_name * jmethod_signature) (* tag = 11 *)
  (** constant values *)
  | ConstString of string  (* tag = 8 *)
  | ConstInt of int32 (* tag = 3 *)
  | ConstFloat of float (* tag = 4 *)
  | ConstLong of int64 (* tag = 5 *)
  | ConstDouble of float (* tag = 6 *)
  (** name and type: used to represent a field or method, without indicating which class it belongs to *)
  | ConstNameAndType of unqualified_name * jsignature
  (** UTF8 encoded strings. Note that when reading/writing, take into account Utf8 modifications of java *)
  (* (http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7) *)
  | ConstUtf8 of string
  (** invokeDynamic-specific *)
  | ConstMethodHandle of (reference_type * jconstant) (* tag = 15 *)
  | ConstMethodType of jmethod_signature (* tag = 16 *)
  | ConstInvokeDynamic of (bootstrap_method * unqualified_name * jsignature) (* tag = 18 *)
  | ConstUnusable

type jcode = unit (* TODO *)

type jaccess_flag =
  | JPublic (* 0x0001 *)
  | JPrivate (* 0x0002 *)
  | JProtected (* 0x0004 *)
  | JStatic (* 0x0008 *)
  | JFinal (* 0x0010 *)
  | JSynchronized (* 0x0020 *)
  | JVolatile (* 0x0040 *)
  | JTransient (* 0x0080 *)
  (** added if created by the compiler *)
  | JSynthetic (* 0x1000 *)
  | JEnum (* 0x4000 *)
  | JUnusable (* should not be present *)
  (** class flags *)
  | JSuper (* 0x0020 *)
  | JInterface (* 0x0200 *)
  | JAbstract (* 0x0400 *)
  | JAnnotation (* 0x2000 *)
  (** method flags *)
  | JBridge (* 0x0040 *)
  | JVarArgs (* 0x0080 *)
  | JNative (* 0x0100 *)
  | JStrict (* 0x0800 *)

type jaccess = jaccess_flag list

(* type parameter name, extends signature, implements signatures *)
type jtypes = (string * jsignature option * jsignature list) list

type jannotation = {
  ann_type : jsignature;
  ann_elements : (string * jannotation_value) list;
}

and jannotation_value =
  | ValConst of jconstant (* B, C, D, E, F, I, J, S, Z, s *)
  | ValEnum of jsignature * string (* e *)
  | ValClass of jsignature (* c *) (* V -> Void *)
  | ValAnnotation of jannotation (* @ *)
  | ValArray of jannotation_value list (* [ *)

type jattribute =
  | AttrDeprecated
  | AttrVisibleAnnotations of jannotation list
  | AttrInvisibleAnnotations of jannotation list
  | AttrUnknown of string * string

type jfield_kind =
  | JKField
  | JKMethod

type jfield = {
  jf_name : string;
  jf_kind : jfield_kind;
  (* signature, as used by the vm *)
  jf_vmsignature : jsignature;
  (* actual signature, as used in java code *)
  jf_signature : jsignature;
  jf_throws : jsignature list;
  jf_types : jtypes;
  jf_flags : jaccess;
  jf_attributes : jattribute list;
  jf_constant : jconstant option;
  jf_code : jcode option;
}

type jclass = {
  cversion : jversion;
  cpath : jpath;
  csuper : jsignature;
  cflags : jaccess;
  cinterfaces : jsignature list;
  cfields : jfield list;
  cmethods : jfield list;
  cattributes : jattribute list;

  cinner_types : (jpath * jpath option * string option * jaccess) list;
  ctypes : jtypes;
}

(* reading/writing *)
type utf8ref = int
type classref = int
type nametyperef = int
type dynref = int
type bootstrapref = int

type jconstant_raw =
  | KClass of utf8ref (* 7 *)
  | KFieldRef of (classref * nametyperef) (* 9 *)
  | KMethodRef of (classref * nametyperef) (* 10 *)
  | KInterfaceMethodRef of (classref * nametyperef) (* 11 *)
  | KString of utf8ref (* 8 *)
  | KInt of int32 (* 3 *)
  | KFloat of float (* 4 *)
  | KLong of int64 (* 5 *)
  | KDouble of float (* 6 *)
  | KNameAndType of (utf8ref * utf8ref) (* 12 *)
  | KUtf8String of string (* 1 *)
  | KMethodHandle of (reference_type * dynref) (* 15 *)
  | KMethodType of utf8ref (* 16 *)
  | KInvokeDynamic of (bootstrapref * nametyperef) (* 18 *)
  | KUnusable

(* jData debugging *)
let is_override_attrib = (function
    (* TODO: pass anotations as @:meta *)
    | AttrVisibleAnnotations ann ->
      List.exists (function
        | { ann_type = TObject( (["java";"lang"], "Override"), [] ) } ->
            true
        | _ -> false
      ) ann
    | _ -> false
  )

let is_override field =
  List.exists is_override_attrib field.jf_attributes

let path_s = function
  | (pack,name) -> String.concat "." (pack @ [name])

let rec s_sig = function
  | TByte (* B *) -> "byte"
  | TChar (* C *) -> "char"
  | TDouble (* D *) -> "double"
  | TFloat (* F *) -> "float"
  | TInt (* I *) -> "int"
  | TLong (* J *) -> "long"
  | TShort (* S *) -> "short"
  | TBool (* Z *) -> "bool"
  | TObject(path,args) -> path_s  path ^ s_args args
  | TObjectInner (sl, sjargl) -> String.concat "." sl ^ "." ^ (String.concat "." (List.map (fun (s,arg) -> s ^ s_args arg) sjargl))
  | TArray (s,i) -> s_sig s ^ "[" ^ (match i with | None -> "" | Some i -> string_of_int i) ^ "]"
  | TMethod (sigs, sopt) -> (match sopt with | None -> "" | Some s -> s_sig s ^ " ") ^ "(" ^ String.concat ", " (List.map s_sig sigs) ^ ")"
  | TTypeParameter s -> s

and s_args = function
  | [] -> ""
  | args -> "<" ^ String.concat ", " (List.map (fun t ->
      match t with
      | TAny -> "*"
      | TType (wc, s) ->
        (match wc with
          | WNone -> ""
          | WExtends -> "+"
          | WSuper -> "-") ^
        (s_sig s))
    args) ^ ">"

let s_field f = (if is_override f then "override " else "") ^ s_sig f.jf_signature ^ " " ^ f.jf_name

let s_fields fs = "{ \n\t" ^ String.concat "\n\t" (List.map s_field fs) ^ "\n}"
*)
