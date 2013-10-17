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
open Printf;;

type machine_type =
	| TUnknown (* 0 - unmanaged PE files only *)
	| Ti386 (* 0x014c - i386 *)
	| TR3000 (* 0x0162 - R3000 MIPS Little Endian *)
	| TR4000 (* 0x0166 - R4000 MIPS Little Endian *)
	| TR10000 (* 0x0168 - R10000 MIPS Little Endian *)
	| TWCEMIPSv2 (* 0x0169 - MIPS Litlte Endian running MS Windows CE 2 *)
	| TAlpha (* 0x0184 - Alpha AXP *)
	| TSH3 (* 0x01a2 - SH3 Little Endian *)
	| TSH3DSP (* 0x01a3 SH3DSP Little Endian *)
	| TSH3E (* 0x01a4 SH3E Little Endian *)
	| TSH4 (* 0x01a6 SH4 Little Endian *)
	| TARM (* 0x1c0 ARM Little Endian *)
	| TARMN (* 0x1c4 ARMv7 (or higher) Thumb mode only Little Endian *)
	| TARM64 (* 0xaa64 - ARMv8 in 64-bit mode *)
	| TThumb (* 0x1c2 ARM processor with Thumb decompressor *)
	| TAM33 (* 0x1d3 AM33 processor *)
	| TPowerPC (* 0x01f0 IBM PowerPC Little Endian *)
	| TPowerPCFP (* 0x01f1 IBM PowerPC with FPU *)
	| TIA64 (* 0x0200 Intel IA64 (Itanium( *)
	| TMIPS16 (* 0x0266 MIPS *)
	| TALPHA64 (* 0x0284 Alpha AXP64 *)
	| TMIPSFPU (* 0x0366 MIPS with FPU *)
	| TMIPSFPU16 (* 0x0466 MIPS16 with FPU *)
	| TTriCore (* 0x0520 Infineon *)
	| TAMD64 (* 0x8664 AMD x64 and Intel E64T *)
	| TM32R (* 0x9041 M32R *)

let machine_type_s m = match m with
	| TUnknown -> "TUnknown"
	| Ti386 -> "Ti386"
	| TR3000 -> "TR3000"
	| TR4000 -> "TR4000"
	| TR10000 -> "TR10000"
	| TWCEMIPSv2 -> "TWCEMIPSv2"
	| TAlpha -> "TAlpha"
	| TSH3 -> "TSH3"
	| TSH3DSP -> "TSH3DSP"
	| TSH3E -> "TSH3E"
	| TSH4 -> "TSH4"
	| TARM -> "TARM"
	| TARMN -> "TARMN"
	| TARM64 -> "TARM64"
	| TThumb -> "TThumb"
	| TAM33 -> "TAM33"
	| TPowerPC -> "TPowerPC"
	| TPowerPCFP -> "TPowerPCFP"
	| TIA64 -> "TIA64"
	| TMIPS16 -> "TMIPS16"
	| TALPHA64 -> "TALPHA64"
	| TMIPSFPU -> "TMIPSFPU"
	| TMIPSFPU16 -> "TMIPSFPU16"
	| TTriCore -> "TTriCore"
	| TAMD64 -> "TAMD64"
	| TM32R -> "TM32R"

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

type pointer = int64

type size_t = pointer

type rva = int32

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

let coff_header_s h =
	sprintf "#COFF_HEADER\n\tmachine: %s\n\tnsections: %d\n\ttimestamp: %ld\n\tsymbol_tbl_pointer: %ld\n\tnsymbols: %d\n\toptheader_size: %d\n\tprops: [%s]\n" (machine_type_s h.coff_machine) h.coff_nsections h.coff_timestamp h.coff_symbol_table_pointer h.coff_nsymbols h.coff_optheader_size (String.concat ", " (List.map coff_prop_s h.coff_props))

let coff_default_exe_props = [ ExecutableImage; LineNumsStripped; LocalSymsStripped; (* Machine32Bit; *) ]

let coff_default_dll_props = [ ExecutableImage; LineNumsStripped; LocalSymsStripped; (* Machine32Bit; *) FileDll ]

type pe_magic =
	| P32 (* 0x10b *)
	| PROM (* 0x107 *)
	| P64 (* 0x20b - called PE32+ on the docs *)
		(* allows 64-bit address space while limiting the image size to 2 gb *)

let pe_magic_s = function
	| P32 -> "P32"
	| PROM -> "PROM"
	| P64 -> "P64"

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

let dll_prop_s = function
	| DDynamicBase -> "DDynamicBase" (* 0x0040 *)
	| DForceIntegrity -> "DForceIntegrity" (* 0x0080 *)
	| DNxCompat -> "DNxCompat" (* 0x0100 *)
	| DNoIsolation -> "DNoIsolation" (* 0x0200 *)
	| DNoSeh -> "DNoSeh" (* 0x0400 *)
	| DNoBind -> "DNoBind" (* 0x0800 *)
	| DWdmDriver -> "DWdmDriver" (* 0x2000 *)
	| DTerminalServer -> "DTerminalServer" (* 0x8000 *)

(* The size of the PE header is not fixed. It depends on the number of data directories defined in the header *)
(* and is specified in the optheader_size in the COFF header *)
(* object files don't have this; but it's required for image files *)
type pe_header = {
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
}

let pe_header_s h =
	sprintf "#PE_HEADER\n\tmagic: %s\n\tmajor/minor %d/%d\n\tsubsystem: %s\n\tdll props: [%s]"
		(pe_magic_s h.pe_magic)
		h.pe_major h.pe_minor
		(subsystem_s h.pe_subsystem)
		(String.concat ", " (List.map dll_prop_s h.pe_dll_props))

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
