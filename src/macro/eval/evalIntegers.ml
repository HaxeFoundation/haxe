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

let encode_haxe_i64 low high =
	let vi = create_instance key_haxe__Int64____Int64 in
	set_instance_field vi key_high (vint32 high);
	set_instance_field vi key_low (vint32 low);
	vinstance vi

let encode_haxe_i64_direct i64 =
	let low = GInt64.to_int32 i64 in
	let high = GInt64.to_int32 (GInt64.shift_right_logical i64 32) in
	encode_haxe_i64 low high

let decode_haxe_i64 v =
	match v with
	| VInstance vi when is v key_haxe__Int64____Int64 ->
		let high = decode_i32 (vi.ifields.(get_instance_field_index_raise vi.iproto key_high))
		and low = decode_i32 (vi.ifields.(get_instance_field_index_raise vi.iproto key_low)) in
		let high64 = GInt64.shift_left (Int32.to_int64 high) 32
		and low64 = GInt64.logand (Int32.to_int64 low) 0xffffffffL in
		GInt64.logor high64 low64
	| _ ->
		unexpected_value v "haxe.Int64"

let decode_u64 = function
	| VUInt64 u -> u
	| v -> unexpected_value v "eval.integers.UInt64"

let decode_i64 = function
	| VInt64 i -> i
	| v -> unexpected_value v "eval.integers.Int64"

let encode_size_t t =
	VUInt64 (UInt64.of_int64 (Size_t.to_int64 t))

let decode_size_t = function
	| VUInt64 u -> Size_t.of_int64 (UInt64.to_int64 u)
	| v -> unexpected_value v "eval.integers.UInt64"

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
	"toInt64", vfun1 (fun v ->
		let u = decode_u64 v in
		VInt64 (Int64.of_int64 (UInt64.to_int64 u))
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
	"MIN", VInt64 Int64.min_int;
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
	"ofHxInt64", vfun1 (fun v ->
		VInt64 (decode_haxe_i64 v)
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
	"toUInt64", vfun1 (fun v ->
		let i = decode_i64 v in
		VUInt64 (UInt64.of_int64 i)
	);
	"toHxInt64", vfun1 (fun v ->
		let i = decode_i64 v in
		encode_haxe_i64_direct i
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
