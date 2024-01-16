open Globals
open Type

exception HxbFailure of string

(*
	MD = module
	MT = module type
	CL = class
	EN = enum
	AB = abstract
	TD = typedef
	AN = anon
	CF = class field
	EF = enum field
	AF = anon field
	EX = expression
	EO = end of (Types | Fields | Module)
	..F = forward definition
	..R = reference
	..D = definition
*)

type chunk_kind =
	| STR (* string pool *)
	| DOC (* doc pool *)
	| MDF (* module foward *)
	| MTF (* module types forward *)
	(* Module type references *)
	| CLR (* class references *)
	| ENR (* enum references *)
	| ABR (* abstract references *)
	| TDR (* typedef references *)
	(* Field references *)
	| AFR (* anon field references *)
	(* Own module type definitions *)
	| CLD (* class definition *)
	| END (* enum definition *)
	| ABD (* abstract definition *)
	| TDD (* typedef definition *)
	| EOT (* end of module types *)
	(* Field references *)
	| EFR (* enum field references *)
	| CFR (* class field references *)
	(* Own field definitions *)
	| CFD (* class fields *)
	| EFD (* enum fields *)
	| AFD (* abstract fields *)
	| EOF (* end of fields *)
	| EXD (* class field expressions *)
	| EOM (* end of module *)

type cached_chunk = chunk_kind * bytes
type cached_chunks = cached_chunk list

type module_cache = {
	mc_path : path;
	mc_id : int;
	mc_chunks : cached_chunks;
	mc_extra : module_def_extra;
}

let string_of_chunk_kind = function
	| STR -> "STR"
	| DOC -> "DOC"
	| MDF -> "MDF"
	| MTF -> "MTF"
	| CLR -> "CLR"
	| ENR -> "ENR"
	| ABR -> "ABR"
	| TDR -> "TDR"
	| AFR -> "AFR"
	| EFR -> "EFR"
	| CFR -> "CFR"
	| CLD -> "CLD"
	| END -> "END"
	| ABD -> "ABD"
	| TDD -> "TDD"
	| EOT -> "EOT"
	| CFD -> "CFD"
	| EFD -> "EFD"
	| AFD -> "AFD"
	| EOF -> "EOF"
	| EXD -> "EXD"
	| EOM -> "EOM"

let chunk_kind_of_string = function
	| "STR" -> STR
	| "DOC" -> DOC
	| "MDF" -> MDF
	| "MTF" -> MTF
	| "CLR" -> CLR
	| "ENR" -> ENR
	| "ABR" -> ABR
	| "TDR" -> TDR
	| "AFR" -> AFR
	| "EFR" -> EFR
	| "CFR" -> CFR
	| "CLD" -> CLD
	| "END" -> END
	| "ABD" -> ABD
	| "TDD" -> TDD
	| "EOT" -> EOT
	| "CFD" -> CFD
	| "EFD" -> EFD
	| "AFD" -> AFD
	| "EOF" -> EOF
	| "EXD" -> EXD
	| "EOM" -> EOM
	| name -> raise (HxbFailure ("Invalid chunk name: " ^ name))

let error (s : string) =
	Printf.eprintf "[error] %s\n" s;
	raise (HxbFailure s)

let hxb_version = 1

let write_header ch =
	IO.nwrite_string ch "hxb";
	IO.write_byte ch hxb_version

let write_chunk_prefix kind length ch =
	IO.nwrite ch (Bytes.unsafe_of_string (string_of_chunk_kind kind));
	IO.write_real_i32 ch (Int32.of_int length)