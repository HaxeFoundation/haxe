module GInt64 = Int64
module GInt32 = Int32

open Globals
open EvalContext
open EvalExceptions
open EvalValue
open EvalEncode
open EvalDecode
open EvalHash
open EvalMisc
open Unsigned
open Signed

let decode_u64 v =
	match v with
	| VUInt64 u -> u
	| _ -> unexpected_value v "eval.integers.UInt64"

let decode_i64 v =
	match v with
	| VInt64 i -> i
	| _ -> unexpected_value v "eval.integers.Int64"

let encode_size_t t =
	VUInt64 (UInt64.of_int64 (Size_t.to_int64 t))

let uint64_fields = [
	"MAX", VUInt64 UInt64.max_int;
	"ZERO", VUInt64 UInt64.zero;
	"ONE", VUInt64 UInt64.one;
	"ofInt", vfun1 (fun v ->
		let i32 = decode_i32 v in
		VUInt64 (UInt64.of_int64 (GInt64.of_int32 i32))
	);
	"ofString", vfun1 (fun v ->
		let s = decode_string v in
		try VUInt64 (UInt64.of_string s)
		with Failure _ -> throw_string "The string is not a valid UInt64 representation" null_pos
	);
	"max", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		VUInt64 (UInt64.max a b)
	);
	"min", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		VUInt64 (UInt64.min a b)
	);
	"compare", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		vint (UInt64.compare a b)
	);
	"toInt", vfun1 (fun v ->
		let u = decode_u64 v in
		vint32 (UInt32.to_int32 (UInt64.to_uint32 u))
	);
	"toString", vfun1 (fun v ->
		let u = decode_u64 v in
		EvalString.vstring (EvalString.create_ascii (UInt64.to_string u))
	);
	"successor", vfun1 (fun v ->
		let u = decode_u64 v in
		VUInt64 (UInt64.succ u)
	);
	"predecessor", vfun1 (fun v ->
		let u = decode_u64 v in
		VUInt64 (UInt64.pred u)
	);
	"remainder", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		try VUInt64 (UInt64.rem a b)
		with e -> throw_string (Printexc.to_string e) null_pos
	);
	"add", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		VUInt64 (UInt64.add a b)
	);
	"sub", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		VUInt64 (UInt64.sub a b)
	);
	"mul", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		VUInt64 (UInt64.mul a b)
	);
	"div", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		try VUInt64 (UInt64.div a b)
		with e -> throw_string (Printexc.to_string e) null_pos
	);
	"logand", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		VUInt64 (UInt64.logand a b)
	);
	"logor", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		VUInt64 (UInt64.logor a b)
	);
	"logxor", vfun2 (fun v1 v2 ->
		let a = decode_u64 v1
		and b = decode_u64 v2 in
		VUInt64 (UInt64.logxor a b)
	);
	"shift_left", vfun2 (fun v1 v2 ->
		let u = decode_u64 v1
		and i = decode_int v2 in
		VUInt64 (UInt64.shift_left u i)
	);
	"shift_right", vfun2 (fun v1 v2 ->
		let u = decode_u64 v1
		and i = decode_int v2 in
		VUInt64 (UInt64.shift_right u i)
	);
	"lognot", vfun1 (fun v ->
		let u = decode_u64 v in
		VUInt64 (UInt64.lognot u)
	);
]

let int64_fields = [
	"MAX", VInt64 Int64.max_int;
	"ZERO", VInt64 Int64.zero;
	"ONE", VInt64 Int64.one;
	"ofInt", vfun1 (fun v ->
		let i32 = decode_i32 v in
		VInt64 (Int64.of_int64 (GInt64.of_int32 i32))
	);
	"ofString", vfun1 (fun v ->
		let s = decode_string v in
		try VInt64 (Int64.of_string s)
		with Failure _ -> throw_string "The string is not a valid Int64 representation" null_pos
	);
	"max", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		VInt64 (Int64.max a b)
	);
	"min", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		VInt64 (Int64.min a b)
	);
	"compare", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		vint (Int64.compare a b)
	);
	"toInt", vfun1 (fun v ->
		let i = decode_i64 v in
		vint32 (GInt64.to_int32 i)
	);
	"toString", vfun1 (fun v ->
		let i = decode_i64 v in
		EvalString.vstring (EvalString.create_ascii (Int64.to_string i))
	);
	"successor", vfun1 (fun v ->
		let i = decode_i64 v in
		VInt64 (Int64.succ i)
	);
	"predecessor", vfun1 (fun v ->
		let i = decode_i64 v in
		VInt64 (Int64.pred i)
	);
	"remainder", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		try VInt64 (Int64.rem a b)
		with e -> throw_string (Printexc.to_string e) null_pos
	);
	"add", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		VInt64 (Int64.add a b)
	);
	"sub", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		VInt64 (Int64.sub a b)
	);
	"mul", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		VInt64 (Int64.mul a b)
	);
	"div", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		try VInt64 (Int64.div a b)
		with e -> throw_string (Printexc.to_string e) null_pos
	);
	"logand", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		VInt64 (Int64.logand a b)
	);
	"logor", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		VInt64 (Int64.logor a b)
	);
	"logxor", vfun2 (fun v1 v2 ->
		let a = decode_i64 v1
		and b = decode_i64 v2 in
		VInt64 (Int64.logxor a b)
	);
	"shift_left", vfun2 (fun v1 v2 ->
		let i64 = decode_i64 v1
		and i = decode_int v2 in
		VInt64 (Int64.shift_left i64 i)
	);
	"shift_right", vfun2 (fun v1 v2 ->
		let i64 = decode_i64 v1
		and i = decode_int v2 in
		VInt64 (Int64.shift_right i64 i)
	);
	"lognot", vfun1 (fun v ->
		let i = decode_i64 v in
		VInt64 (Int64.lognot i)
	);
]