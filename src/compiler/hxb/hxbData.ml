open Globals
open Type

exception HxbFailure of string

(* TEMP: Wipe server cache to force loading from hxb *)
(* See ServerCompilationContext.after_compilation *)
(* Also see ServerTests.testDisplayModuleRecache test which needs updating if set to false *)
let always_wipe_cache = true

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
	...F = forward definition
	...R = reference
	...D = definition
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
	| EFR (* enum field references *)
	| CFR (* class field references *)
	(* Own module type definitions *)
	| CLD (* class definition *)
	| END (* enum definition *)
	| ABD (* abstract definition *)
	| TDD (* typedef definition *)
	(* Own field definitions *)
	| CFD (* class fields *)
	| EFD (* enum fields *)
	| AFD (* abstract fields *)
	| EXD (* class field expressions *)
	| LST (* last *)

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
	| CFD -> "CFD"
	| EFD -> "EFD"
	| AFD -> "AFD"
	| EXD -> "EXD"
	| LST -> "LST"

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
	| "CFD" -> CFD
	| "EFD" -> EFD
	| "AFD" -> AFD
	| "EXD" -> EXD
	| "LST" -> LST
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