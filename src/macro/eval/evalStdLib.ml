(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)
open Extlib_leftovers
open Globals
open EvalValue
open EvalEncode
open EvalDecode
open EvalContext
open EvalExceptions
open EvalPrinting
open EvalMisc
open EvalField
open EvalHash
open EvalString
open EvalThread

let catch_unix_error f arg =
	try
		f arg
	with Unix.Unix_error(err,cmd,args) ->
		exc_string (Printf.sprintf "%s(%s, %s)" (Unix.error_message err) cmd args)

let ptmap_keys h =
	IntMap.fold (fun k _ acc -> k :: acc) h []

let hashtbl_keys h =
	Hashtbl.fold (fun k _ acc -> k :: acc) h []

module StdEvalVector = struct
	let this this = match this with
		| VVector vv -> vv
		| v -> unexpected_value v "vector"

	let blit = vifun4 (fun vthis srcPos dest destPos len ->
		Array.blit (this vthis) (decode_int srcPos) (decode_vector dest) (decode_int destPos) (decode_int len);
		vnull
	)

	let toArray = vifun0 (fun vthis ->
		let copy = Array.copy (this vthis) in
		encode_array_instance (EvalArray.create copy)
	)

	let fromArrayCopy = vfun1 (fun arr ->
		let a = decode_varray arr in
		encode_vector_instance (Array.sub a.avalues 0 a.alength)
	)

	let copy = vifun0 (fun vthis ->
		encode_vector_instance (Array.copy (this vthis))
	)

	let join = vifun1 (fun vthis sep ->
		let this = this vthis in
		let sep = decode_vstring sep in
		vstring ((EvalArray.array_join this (s_value 0) sep))
	)

	let map = vifun1 (fun vthis f ->
		let this = this vthis in
		let a = match f with
			| VFunction(f,_) ->
				Array.map (fun v -> f [v]) this
			| VFieldClosure(v1,f) ->
				Array.map (fun v -> f (v1 :: [v])) this
			| _ -> exc_string ("Cannot call " ^ (value_string f))
		in
		encode_vector_instance a
	)
end

module StdArray = struct
	let this this = match this with
		| VArray va -> va
		| v -> unexpected_value v "array"

	let concat = vifun1 (fun vthis a2 ->
		let a2 = decode_varray a2 in
		encode_array_instance (EvalArray.concat (this vthis) a2)
	)

	let copy = vifun0 (fun vthis ->
		encode_array_instance (EvalArray.copy (this vthis))
	)

	let filter = vifun1 (fun vthis f ->
		let this = this vthis in
		let a = EvalArray.filter this (fun v -> is_true (call_value_on vthis f [v])) in
		encode_array_instance a
	)

	let indexOf = vifun2 (fun vthis x fromIndex ->
		let this = this vthis in
		let fromIndex = default_int fromIndex 0 in
		let fromIndex = if fromIndex < 0 then this.alength + fromIndex else fromIndex in
		let fromIndex = if fromIndex < 0 then 0 else fromIndex in
		vint (EvalArray.indexOf this equals x fromIndex)
	)

	let insert = vifun2 (fun vthis pos x ->
		let this = this vthis in
		let pos = decode_int pos in
		if pos >= this.alength then begin
			ignore(EvalArray.push this x);
		end else begin
			let pos = if pos < 0 then this.alength + pos else pos in
			let pos = if pos < 0 then 0 else pos in
			EvalArray.insert this pos x
		end;
		vnull
	)

	let iterator = vifun0 (fun vthis ->
		let this = this vthis in
		let f_has_next,f_next = EvalArray.iterator this in
		encode_obj [
			key_hasNext,vifun0 (fun _ -> vbool (f_has_next()));
			key_next,vifun0 (fun _ -> f_next())
		]
	)

	let join = vifun1 (fun vthis sep ->
		let sep = decode_vstring sep in
		let s = EvalArray.join (this vthis) (s_value 0) sep in
		vstring s
	)

	let keyValueIterator = vifun0 (fun vthis ->
		let ctx = get_ctx() in
		let path = key_haxe_iterators_array_key_value_iterator in
		let vit = encode_instance path in
		let fnew = get_instance_constructor ctx path null_pos in
		ignore(call_value_on vit (Lazy.force fnew) [vthis]);
		vit
	)

	let lastIndexOf = vifun2 (fun vthis x fromIndex ->
		let this = this vthis in
		let last = this.alength - 1 in
		let fromIndex = default_int fromIndex last in
		let fromIndex = if fromIndex < 0 then this.alength + fromIndex else fromIndex in
		let fromIndex = if fromIndex < 0 then 0 else if fromIndex > last then last else fromIndex in
		vint (EvalArray.lastIndexOf this equals x fromIndex)
	)

	let map = vifun1 (fun vthis f ->
		let this = this vthis in
		let a = match f with
			| VFunction(f,_) ->
				EvalArray.map this (fun v -> f [v])
			| VFieldClosure(v1,f) ->
				EvalArray.map this (fun v -> f (v1 :: [v]))
			| _ -> exc_string ("Cannot call " ^ (value_string f))
		in
		encode_array_instance a
	)

	let pop = vifun0 (fun vthis ->
		let this = this vthis in
		EvalArray.pop this
	)

	let push = vifun1 (fun vthis v ->
		let this = this vthis in
		vint32 (Int32.of_int (EvalArray.push this v))
	)

	let remove = vifun1 (fun vthis x ->
		let this = this vthis in
		vbool (EvalArray.remove this equals x)
	)

	let contains = vifun1 (fun vthis x ->
		let this = this vthis in
		vbool (EvalArray.contains this equals x)
	)

	let reverse = vifun0 (fun vthis ->
		let this = this vthis in
		EvalArray.reverse this;
		vnull
	)

	let shift = vifun0 (fun vthis ->
		let this = this vthis in
		EvalArray.shift this
	)

	let slice = vifun2 (fun vthis pos end' ->
		let this = this vthis in
		let pos = decode_int pos in
		let length = this.alength in
		let end' = default_int end' length in
		let end' = if end' > length then length else end' in
		let pos = if pos < 0 then length + pos else pos in
		let end' = if end' < 0 then length + end' else end' in
		let pos = if pos < 0 then 0 else pos in
		let end' = if end' < 0 then 0 else end' in
		encode_array_instance (EvalArray.slice this pos end')
	)

	let sort = vifun1 (fun vthis f ->
		let this = this vthis in
		EvalArray.sort this (fun a b -> decode_int (call_value_on vthis f [a;b]));
		vnull
	)

	let splice = vifun2 (fun vthis pos len ->
		let this = this vthis in
		let pos = decode_int pos in
		let len = decode_int len in
		let length = this.alength in
		if len < 0 || pos > length then
			encode_array []
		else begin
			let pos = if pos < 0 then length + pos else pos in
			let pos = if pos < 0 then 0 else pos in
			let delta = length - pos in
			let len = if len > delta then delta else len in
			let end' = pos + len in
			encode_array_instance (EvalArray.splice this pos len end')
		end
	)

	let toString = vifun0 (fun vthis ->
		vstring (s_array 0 0 (this vthis))
	)

	let unshift = vifun1 (fun vthis v ->
		let this = this vthis in
		EvalArray.unshift this v;
		vnull
	)

	let resize = vifun1 (fun vthis len ->
		let this = this vthis in
		let len = decode_int len in
		EvalArray.resize this len;
		vnull
	)
end

let outside_bounds () =
	let haxe_io_Error = get_static_prototype (get_ctx()) key_haxe_io_Error null_pos in
	exc (proto_field_direct haxe_io_Error key_OutsideBounds)

module StdBytes = struct
	open EvalBytes

	let this vthis = match vthis with
		| VInstance {ikind = IBytes o} -> o
		| v -> unexpected_value v "bytes"

	let alloc = vfun1 (fun length ->
		let length = decode_int length in
		encode_bytes (Bytes.make length (Char.chr 0))
	)

	let encode_native v = match v with
		| VEnumValue {eindex = 1} -> true (* haxe.io.Encoding.RawNative *)
		| _ -> false

	let blit = vifun4 (fun vthis pos src srcpos len ->
		let s = this vthis in
		let pos = decode_int pos in
		let src = decode_bytes src in
		let srcpos = decode_int srcpos in
		let len = decode_int len in
		(try Bytes.blit src srcpos s pos len with _ -> outside_bounds());
		vnull
	)

	let compare = vifun1 (fun vthis other ->
		let this = this vthis in
		let other = decode_bytes other in
		vint (Stdlib.compare this other)
	)

	let fastGet = vfun2 (fun b pos ->
		let b = decode_bytes b in
		let pos = decode_int pos in
		try vint (int_of_char (Bytes.unsafe_get b pos)) with _ -> vnull
	)

	let fill = vifun3 (fun vthis pos len value ->
		let this = this vthis in
		let pos = decode_int pos in
		let len = decode_int len in
		let value = decode_int value in
		(try Bytes.fill this pos len (char_of_int (value land 0xFF)) with _ -> outside_bounds());
		vnull
	)

	let get = vifun1 (fun vthis pos ->
		let this = this vthis in
		let pos = decode_int pos in
		try vint (read_byte this pos) with _ -> vnull
	)

	let getData = vifun0 (fun vthis -> vthis)

	let getDouble = vifun1 (fun vthis pos ->
		try vfloat (Int64.float_of_bits (read_i64 (this vthis) (decode_int pos))) with _ -> outside_bounds()
	)

	let getFloat = vifun1 (fun vthis pos ->
		try vfloat (Int32.float_of_bits (read_i32 (this vthis) (decode_int pos))) with _ -> outside_bounds()
	)

	let getInt32 = vifun1 (fun vthis pos ->
		try vint32 (read_i32 (this vthis) (decode_int pos)) with exc -> outside_bounds()
	)

	let getInt64 = vifun1 (fun vthis pos ->
		let this = this vthis in
		let pos = decode_int pos in
		try
			let low = read_i32 this pos in
			let high = read_i32 this (pos + 4) in
			EvalIntegers.encode_haxe_i64 low high;
		with _ ->
			outside_bounds()
	)

	let getString = vifun3 (fun vthis pos len encoding ->
		let this = this vthis in
		let pos = decode_int pos in
		let len = decode_int len in
		let s = try Bytes.sub this pos len with _ -> outside_bounds() in
		create_unknown (Bytes.unsafe_to_string s)
	)

	let getUInt16 = vifun1 (fun vthis pos ->
		try vint (read_ui16 (this vthis) (decode_int pos)) with _ -> outside_bounds()
	)

	let ofData = vfun1 (fun v -> v)

	let ofString = vfun2 (fun v encoding ->
		let s = decode_vstring v in
		encode_bytes (Bytes.of_string s.sstring)
	)

	let ofHex = vfun1 (fun v ->
		let s = decode_string v in
		let len = String.length s in
		if (len land 1) <> 0 then exc_string "Not a hex string (odd number of digits)";
		let ret = (Bytes.make (len lsr 1) (Char.chr 0)) in
		for i = 0 to Bytes.length ret - 1 do
			let high = int_of_char s.[i * 2] in
			let low = int_of_char s.[i * 2 + 1] in
			let high = (high land 0xF) + ((high land 0x40) lsr 6) * 9 in
			let low = (low land 0xF) + ((low land 0x40) lsr 6) * 9 in
			Bytes.set ret i (char_of_int (((high lsl 4) lor low) land 0xFF));
		done;
		encode_bytes ret
	)

	let set = vifun2 (fun vthis pos v ->
		let this = this vthis in
		let pos = decode_int pos in
		let v = decode_int v in
		(try write_byte this pos v with _ -> ());
		vnull;
	)

	let setDouble = vifun2 (fun vthis pos v ->
		(try write_i64 (this vthis) (decode_int pos) (Int64.bits_of_float (num v)) with _ -> outside_bounds());
		vnull
	)

	let setFloat = vifun2 (fun vthis pos v ->
		let this = this vthis in
		let pos = decode_int pos in
		let v = num v in
		write_i32 this pos (Int32.bits_of_float v);
		vnull
	)

	let setInt32 = vifun2 (fun vthis pos v ->
		(try write_i32 (this vthis) (decode_int pos) (decode_i32 v) with _ -> outside_bounds());
		vnull;
	)

	let setInt64 = vifun2 (fun vthis pos v ->
		let v = decode_instance v in
		let pos = decode_int pos in
		let high = decode_i32 (instance_field v key_high) in
		let low = decode_i32 (instance_field v key_low) in
		let this = this vthis in
		try
			write_i32 this pos low;
			write_i32 this (pos + 4) high;
			vnull
		with _ ->
			outside_bounds()
	)

	let setUInt16 = vifun2 (fun vthis pos v ->
		(try write_ui16 (this vthis) (decode_int pos) (decode_int v land 0xFFFF) with _ -> outside_bounds());
		vnull
	)

	let sub = vifun2 (fun vthis pos len ->
		let this = this vthis in
		let pos = decode_int pos in
		let len = decode_int len in
		let s = try Bytes.sub this pos len with _ -> outside_bounds() in
		encode_bytes s
	)

	let toHex = vifun0 (fun vthis ->
		let this = this vthis in
		let chars = [|"0";"1";"2";"3";"4";"5";"6";"7";"8";"9";"a";"b";"c";"d";"e";"f"|] in
		let l = Bytes.length this in
		let rec loop acc i =
			if i >= l then List.rev acc
			else begin
				let c = int_of_char (Bytes.get this i) in
				loop ((chars.(c land 15)) :: ((chars.(c lsr 4))) :: acc) (i + 1)
			end
		in
		encode_string (String.concat "" (loop [] 0))
	)

	let toString = vifun0 (fun vthis ->
		let this = this vthis in
		try
			UTF8.validate (Bytes.unsafe_to_string this);
			(create_unknown (Bytes.to_string this))
		with UTF8.Malformed_code ->
			exc_string "Invalid string"
	)
end

module StdBytesBuffer = struct
	let this vthis = match vthis with
		| VInstance {ikind = IOutput o} -> o
		| v -> unexpected_value v "output"

	let get_length = vifun0 (fun vthis ->
		let this = this vthis in
		vint (Buffer.length this)
	)

	let add_char this i =
		Buffer.add_char this (Char.unsafe_chr i)

	let add_i32 this v =
		let base = Int32.to_int v in
		let big = Int32.to_int (Int32.shift_right_logical v 24) in
		add_char this base;
		add_char this (base lsr 8);
		add_char this (base lsr 16);
		add_char this big

	let addByte = vifun1 (fun vthis byte ->
		let this = this vthis in
		let byte = decode_int byte in
		add_char this byte;
		vnull;
	)

	let add = vifun1 (fun vthis src ->
		let this = this vthis in
		let src = decode_bytes src in
		Buffer.add_bytes this src;
		vnull
	)

	let addString = vifun2 (fun vthis src encoding ->
		let this = this vthis in
		let src = decode_vstring src in
		Buffer.add_string this src.sstring;
		vnull
	)

	let addInt32 = vifun1 (fun vthis v ->
		let this = this vthis in
		let v = decode_i32 v in
		add_i32 this v;
		vnull
	)

	let addInt64 = vifun1 (fun vthis v ->
		let this = this vthis in
		let v = decode_instance v in
		let high = decode_i32 (instance_field v key_high) in
		let low = decode_i32 (instance_field v key_low) in
		add_i32 this low;
		add_i32 this high;
		vnull;
	)

	let addFloat = vifun1 (fun vthis v ->
		let this = this vthis in
		let v = num v in
		add_i32 this (Int32.bits_of_float v);
		vnull
	)

	let addDouble = vifun1 (fun vthis v ->
		let this = this vthis in
		let v = num v in
		let v = Int64.bits_of_float v in
		add_i32 this (Int64.to_int32 v);
		add_i32 this (Int64.to_int32 (Int64.shift_right_logical v 32));
		vnull
	)

	let addBytes = vifun3 (fun vthis src pos len ->
		let this = this vthis in
		let src = decode_bytes src in
		let pos = decode_int pos in
		let len = decode_int len in
		if pos < 0 || len < 0 || pos + len > Bytes.length src then outside_bounds();
		Buffer.add_subbytes this src pos len;
		vnull
	)

	let getBytes = vifun0 (fun vthis ->
		let this = this vthis in
		encode_bytes (Bytes.unsafe_of_string (Buffer.contents this))
	)
end

module StdCompress = struct
	open Extc

	type zfun = zstream -> src:string -> spos:int -> slen:int -> dst:bytes -> dpos:int -> dlen:int -> zflush -> zresult

	let this vthis = match vthis with
		| VInstance {ikind = IZip zip} -> zip
		| _ -> unexpected_value vthis "Compress"

	let exec (f : zfun) vthis src srcPos dst dstPos =
		let this = this vthis in
		let src = decode_bytes src in
		let srcPos = decode_int srcPos in
		let dst = decode_bytes dst in
		let dstPos = decode_int dstPos in
		let r = try f this.z (Bytes.unsafe_to_string src) srcPos (Bytes.length src - srcPos) dst dstPos (Bytes.length dst - dstPos) this.z_flush with _ -> exc_string "oops" in
		encode_obj [
			key_done,vbool r.z_finish;
			key_read,vint r.z_read;
			key_write,vint r.z_wrote
		]

	let close = vifun0 (fun vthis ->
		zlib_deflate_end (this vthis).z;
		vnull
	)

	let execute = vifun4 (fun vthis src srcPos dst dstPos ->
		exec zlib_deflate vthis src srcPos dst dstPos
	)

	let run = vfun2 (fun s level ->
		let s = decode_bytes s in
		let level = decode_int level in
		let zip = zlib_deflate_init level in
		let d = Bytes.make (zlib_deflate_bound zip (Bytes.length s)) (char_of_int 0) in
		let r = zlib_deflate zip (Bytes.unsafe_to_string s) 0 (Bytes.length s) d 0 (Bytes.length d) Z_FINISH in
		zlib_deflate_end zip;
		if not r.z_finish || r.z_read <> (Bytes.length s) then exc_string "Compression failed";
		encode_bytes (Bytes.sub d 0 r.z_wrote)
	)

	let setFlushMode = vifun1 (fun vthis f ->
		let mode = match fst (decode_enum f) with
			| 0 -> Z_NO_FLUSH
			| 1 -> Z_SYNC_FLUSH
			| 2 -> Z_FULL_FLUSH
			| 3 -> Z_FINISH
			| 4 -> Z_PARTIAL_FLUSH
			| _ -> die "" __LOC__
		in
		(this vthis).z_flush <- mode;
		vnull
	)
end

module StdContext = struct
	let addBreakpoint = vfun2 (fun file line ->
		let file = decode_string file in
		let line = decode_int line in
		begin try
			ignore(EvalDebugMisc.add_breakpoint (get_ctx()) file line BPAny None);
		with Not_found ->
			exc_string ("Could not find file " ^ file)
		end;
		vnull
	)

	let breakHere = vfun0 (fun () ->
		if not ((get_ctx()).debug.support_debugger) then vnull
		else raise (EvalDebugMisc.BreakHere)
	)

	let callMacroApi = vfun1 (fun f  ->
		let f = decode_string f in
		try
			Hashtbl.find GlobalState.macro_lib f
		with Not_found ->
			exc_string ("Could not find macro function \"" ^ f ^ "\"")
	)

	let plugins = ref PMap.empty

	let plugin_data = ref None

	let register data = plugin_data := Some data

	let loadPlugin = vfun1 (fun filePath ->
		let filePath = decode_string filePath in
		let filePath = Dynlink.adapt_filename filePath in
		if PMap.mem filePath !plugins then
			PMap.find filePath !plugins
		else begin
			(try Dynlink.loadfile filePath with Dynlink.Error error -> exc_string (Dynlink.error_message error));
			match !plugin_data with
				| Some l ->
					let vapi = encode_obj_s l in
					plugins := PMap.add filePath vapi !plugins;
					vapi
				| None ->
					vnull
		end
	)
end

module StdCrc32 = struct
	let make = vfun1 (fun data ->
		let data = decode_bytes data in
		let crc32 = Extc.zlib_crc32 data (Bytes.length data) in
		vint32 crc32
	)
end

module StdDate = struct
	open Unix

	let encode_date d = encode_instance key_Date ~kind:(IDate d)

	let this vthis = match vthis with
		| VInstance {ikind = IDate d} -> d
		| v -> unexpected_value v "date"

	let fromTime = vfun1 (fun f -> encode_date ((num f) /. 1000.))

	let fromString = vfun1 (fun s ->
		let s = decode_string s in
		match String.length s with
		| 19 ->
			let r = Str.regexp "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)$" in
			if not (Str.string_match r s 0) then exc_string ("Invalid date format : " ^ s);
			let t = {
				tm_year = int_of_string (Str.matched_group 1 s) - 1900;
				tm_mon = int_of_string (Str.matched_group 2 s) - 1;
				tm_mday = int_of_string (Str.matched_group 3 s);
				tm_hour = int_of_string (Str.matched_group 4 s);
				tm_min = int_of_string (Str.matched_group 5 s);
				tm_sec = int_of_string (Str.matched_group 6 s);
				tm_wday = 0;
				tm_yday = 0;
				tm_isdst = false;
			} in
			encode_date (fst (catch_unix_error mktime t))
		| 10 ->
			let r = Str.regexp "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$" in
			if not (Str.string_match r s 0) then exc_string ("Invalid date format : " ^ s);
			let t = {
				tm_year = int_of_string (Str.matched_group 1 s) - 1900;
				tm_mon = int_of_string (Str.matched_group 2 s) - 1;
				tm_mday = int_of_string (Str.matched_group 3 s);
				tm_hour = 0;
				tm_min = 0;
				tm_sec = 0;
				tm_wday = 0;
				tm_yday = 0;
				tm_isdst = false;
			} in
			encode_date (fst (catch_unix_error mktime t))
		| 8 ->
			let r = Str.regexp "^\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)$" in
			if not (Str.string_match r s 0) then exc_string ("Invalid date format : " ^ s);
			let h = int_of_string (Str.matched_group 1 s) in
			let m = int_of_string (Str.matched_group 2 s) in
			let s = int_of_string (Str.matched_group 3 s) in
			let t = h * 60 * 60 + m * 60 + s in
			encode_date (float_of_int t)
		| _ ->
			exc_string ("Invalid date format : " ^ s)
	)

	let getDate = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_mday)
	let getDay = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_wday)
	let getFullYear = vifun0 (fun vthis -> vint (((catch_unix_error localtime (this vthis)).tm_year) + 1900))
	let getHours = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_hour)
	let getMinutes = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_min)
	let getMonth = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_mon)
	let getSeconds = vifun0 (fun vthis -> vint (catch_unix_error localtime (this vthis)).tm_sec)
	let getUTCDate = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_mday)
	let getUTCDay = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_wday)
	let getUTCFullYear = vifun0 (fun vthis -> vint (((catch_unix_error gmtime (this vthis)).tm_year) + 1900))
	let getUTCHours = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_hour)
	let getUTCMinutes = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_min)
	let getUTCMonth = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_mon)
	let getUTCSeconds = vifun0 (fun vthis -> vint (catch_unix_error gmtime (this vthis)).tm_sec)
	let getTime = vifun0 (fun vthis -> vfloat ((this vthis) *. 1000.))
	let getTimezoneOffset = vifun0 (fun vthis ->
		let tmLocal = catch_unix_error localtime (this vthis) in
		let tmUTC = catch_unix_error gmtime (this vthis) in
		let tsLocal = fst (catch_unix_error mktime tmLocal) in
		let tsUTC = fst (catch_unix_error mktime tmUTC) in
		vint (int_of_float ((tsUTC -. tsLocal) /. 60.))
	)
	let now = vfun0 (fun () -> encode_date (catch_unix_error time()))
	let toString = vifun0 (fun vthis -> vstring (s_date (this vthis)))
end

module StdDeque = struct
	let this vthis = match vthis with
		| VInstance {ikind = IDeque d} -> d
		| _ -> unexpected_value vthis "Deque"

	let add = vifun1 (fun vthis i ->
		let this = this vthis in
		Deque.add this i;
		vnull
	)

	let pop = vifun1 (fun vthis blocking ->
		let this = this vthis in
		let blocking = decode_bool blocking in
		match Deque.pop this blocking with
		| None -> vnull
		| Some v -> v
	)

	let push = vifun1 (fun vthis i ->
		let this = this vthis in
		Deque.push this i;
		vnull
	)
end

module StdEReg = struct
	open Pcre2

	let create r opt =
		let open Pcre2 in
		let string_of_pcre_error = function
			| BadPattern(s,i) -> Printf.sprintf "at %i: %s" i s
			| Partial -> "Partial"
			| BadUTF -> "BadUTF"
			| BadUTFOffset -> "BadUTFOffset"
			| MatchLimit -> "MatchLimit"
			| DepthLimit -> "DepthLimit"
			| WorkspaceSize -> "WorkspaceSize"
			| InternalError s -> "InternalError: " ^ s
		in
		let global = ref false in
		let flags = ExtList.List.filter_map (function
			| 'i' -> Some `CASELESS
			| 's' -> Some `DOTALL
			| 'm' -> Some `MULTILINE
			| 'u' -> None
			| 'g' -> global := true; None
			| c -> failwith ("Unsupported regexp option '" ^ String.make 1 c ^ "'")
		) (ExtString.String.explode opt) in
		let flags = `UTF :: `UCP :: flags in
		let rex = try regexp ~flags r with Error error -> failwith (string_of_pcre_error error) in
		let pcre = {
			r = rex;
			r_rex_string = create_ascii (Printf.sprintf "~/%s/%s" r opt);
			r_global = !global;
			r_string = "";
			r_groups = [||]
		} in
		IRegex pcre

	let maybe_run rex n f =
		let substrings = if Array.length rex.r_groups = 0 then exc_string "Invalid regex operation because no match was made" else rex.r_groups.(0) in
		if n < 0 || n >= num_of_subs substrings then exc_string "Invalid group"
		else try f (get_substring_ofs substrings n)
		with Not_found -> vnull

	let this this = match this with
		| VInstance {ikind = IRegex rex} -> rex
		| v -> unexpected_value v "EReg"

	let escape = vfun1 (fun s ->
		let s = decode_string s in
		create_unknown (Str.quote s)
	)

	let map = vifun2 (fun vthis s f ->
		let this = this vthis in
		let s = decode_string s in
		let l = String.length s in
		let buf = Buffer.create 0 in
		let rec loop pos =
			if pos >= l then
				()
			else begin try
				let a = exec ~rex:this.r ~pos s in
				this.r_groups <- [|a|];
				let (first,last) = get_substring_ofs a 0 in
				Buffer.add_substring buf s pos (first - pos);
				Buffer.add_string buf (decode_string (call_value_on vthis f [vthis]));
				if last = first then begin
					if last >= l then
						()
					else begin
						if this.r_global then begin
							Buffer.add_substring buf s first 1;
							loop (first + 1)
						end else
							Buffer.add_substring buf s first (l - first)
					end
				end else if this.r_global then
					loop last
				else
					Buffer.add_substring buf s last (l - last)
			with Not_found ->
				Buffer.add_substring buf s pos (l - pos)
			end
		in
		this.r_string <- s;
		loop 0;
		this.r_string <- "";
		this.r_groups <- [||];
		create_unknown (Buffer.contents buf)
	)

	let match' = vifun1 (fun vthis s ->
		let this = this vthis in
		let open Pcre2 in
		let s = decode_string s in
		this.r_string <- s;
		try
			let a = exec_all ~flags:[`NO_UTF_CHECK] ~rex:this.r s in
			this.r_groups <- a;
			vtrue
		with Not_found ->
			this.r_groups <- [||];
			vfalse
		| Pcre2.Error _ ->
			exc_string "PCRE Error (invalid unicode string?)"
	)

	let matched = vifun1 (fun vthis n ->
		let this = this vthis in
		let n = decode_int n in
		maybe_run this n (fun (first,last) ->
			create_unknown (ExtString.String.slice ~first ~last this.r_string)
		)
	)

	let matchedLeft = vifun0 (fun vthis ->
		let this = this vthis in
		maybe_run this 0 (fun (first,_) ->
			create_unknown (ExtString.String.slice ~last:first this.r_string)
		)
	)

	let matchedPos = vifun0 (fun vthis ->
		let this = this vthis in
		let rec byte_offset_to_char_offset_lol s i k o =
			if i = 0 then
				k
			else begin
				let n = UTF8.next s o in
				let d = n - o in
				byte_offset_to_char_offset_lol s (i - d) (k + 1) n
			end
		in
		maybe_run this 0 (fun (first,last) ->
			let first = byte_offset_to_char_offset_lol this.r_string first 0 0 in
			let last = byte_offset_to_char_offset_lol this.r_string last 0 0 in
			encode_obj [key_pos,vint first;key_len,vint (last - first)]
		)
	)

	let matchedRight = vifun0 (fun vthis ->
		let this = this vthis in
		maybe_run this 0 (fun (_,last) ->
			create_unknown (ExtString.String.slice ~first:last this.r_string)
		)
	)

	let matchSub = vifun3 (fun vthis s pos len ->
		let this = this vthis in
		let s = decode_string s in
		let pos = decode_int pos in
		let len_default = String.length s - pos in
		let len = default_int len len_default in
		let len = if len < 0 then len_default else len in
		begin try
			if pos + len > String.length s then raise Not_found;
			let str = String.sub s 0 (pos + len) in
			let a = Pcre2.exec_all ~flags:[`NO_UTF_CHECK] ~rex:this.r ~pos str in
			this.r_string <- s;
			this.r_groups <- a;
			vtrue
		with Not_found ->
			vfalse
		end
	)

	let replace = vifun2 (fun vthis s by ->
		let this = this vthis in
		let s = decode_string s in
		let by = decode_string by in
		let s = (if this.r_global then Pcre2.replace else Pcre2.replace_first) ~flags:[`NO_UTF_CHECK] ~rex:this.r ~templ:by s in
		create_unknown s
	)

	let split = vifun1 (fun vthis s ->
		let this = this vthis in
		let s = decode_string s in
		let slength = String.length s in
		if slength = 0 then
			encode_array [v_empty_string]
		else begin
			let copy_offset = ref 0 in
			let acc = DynArray.create () in
			let add first last =
				let sub = String.sub s first (last - first) in
				DynArray.add acc (create_unknown sub)
			in
			let exec = Pcre2.exec ~flags:[`NO_UTF_CHECK] ~rex:this.r in
			let step pos =
				try
					let substrings = exec ~pos s in
					let (first,last) = Pcre2.get_substring_ofs substrings 0 in
					add !copy_offset first;
					copy_offset := last;
					let next_start = if pos = last then last + 1 else last in
					if next_start >= slength then begin
						DynArray.add acc (create_unknown "");
						None
					end else
						Some next_start
				with Not_found ->
					add !copy_offset slength;
					None
			in
			let rec loop pos =
				match step pos with
				| Some next ->
					if this.r_global then
						loop next
					else
						add !copy_offset slength
				| _ ->
					()
			in
			loop 0;
			encode_array (DynArray.to_list acc)
		end
	)
end

module StdFile = struct
	let create_out path binary flags =
		let path = decode_string path in
		let binary = match binary with
			| VTrue | VNull -> true
			| _ -> false
		in
		let perms = 0o666 in
		let l = Open_creat :: flags in
		let l = if binary then Open_binary :: l else l in
		let ch =
			try open_out_gen l perms path
			with Sys_error msg -> exc_string msg
		in
		encode_instance key_sys_io_FileOutput ~kind:(IOutChannel ch)

	let write_out path content =
		try
			let ch = open_out_bin path in
			output_string ch content;
			close_out ch;
			vnull
		with Sys_error s ->
			exc_string s

	let append = vfun2 (fun path binary ->
		create_out path binary [Open_append]
	)

	let update = vfun2 (fun path binary ->
		create_out path binary [Open_rdonly; Open_wronly]
	)

	let getBytes = vfun1 (fun path ->
		let path = decode_string path in
		try encode_bytes (Bytes.unsafe_of_string (Std.input_file ~bin:true path)) with Sys_error _ -> exc_string ("Could not read file " ^ path)
	)

	let getContent = vfun1 (fun path ->
		let path = decode_string path in
		try ((create_unknown (Std.input_file ~bin:true path))) with Sys_error _ -> exc_string ("Could not read file " ^ path)
	)

	let read = vfun2 (fun path binary ->
		let path = decode_string path in
		let binary = match binary with
			| VTrue | VNull -> true
			| _ -> false
		in
		let ch =
			try open_in_gen (Open_rdonly :: (if binary then [Open_binary] else [])) 0 path
			with Sys_error msg -> exc_string msg
		in
		encode_instance key_sys_io_FileInput ~kind:(IInChannel(ch,ref false))
	)

	let saveBytes = vfun2 (fun path bytes ->
		let path = decode_string path in
		let bytes = decode_bytes bytes in
		write_out path (Bytes.unsafe_to_string bytes)
	)

	let saveContent = vfun2 (fun path content ->
		let path = decode_string path in
		let content = decode_string content in
		write_out path content
	)

	let write = vfun2 (fun path binary ->
		create_out path binary [Open_wronly;Open_trunc]
	)
end

module StdFileInput = struct
	let raise_eof () =
		let v = encode_instance key_haxe_io_Eof in
		exc v

	let this vthis = match vthis with
		| VInstance {ikind = IInChannel(ch,eof)} -> ch,eof
		| _ -> unexpected_value vthis "FileInput"

	let close = vifun0 (fun vthis ->
		close_in (fst (this vthis));
		vnull
	)

	let eof = vifun0 (fun vthis ->
		vbool !(snd (this vthis))
	)

	let seek = vifun2 (fun vthis pos mode ->
		let ch,r = this vthis in
		r := false;
		let pos = decode_int pos in
		let mode,_ = decode_enum mode in
		seek_in ch (match mode with 0 -> pos | 1 -> pos_in ch + pos | 2 -> in_channel_length ch + pos | _ -> die "" __LOC__);
		vnull
	)

	let tell = vifun0 (fun vthis ->
		vint (pos_in (fst (this vthis)))
	)

	let readByte = vifun0 (fun vthis ->
		let ch,r = this vthis in
		let i = try
			input_char ch
		with _ ->
			r := true;
			raise_eof()
		in
		vint (int_of_char i)
	)

	let readBytes = vifun3 (fun vthis bytes pos len ->
		let ch,r = this vthis in
		let bytes = decode_bytes bytes in
		let pos = decode_int pos in
		let len = decode_int len in
		let i = input ch bytes pos len in
		if i = 0 then begin
			r := true;
			raise_eof()
		end;
		vint i
	)
end

module StdFileOutput = struct
	let this vthis = match vthis with
		| VInstance {ikind = IOutChannel ch} -> ch
		| _ -> unexpected_value vthis "FileOutput"

	let close = vifun0 (fun vthis ->
		close_out (this vthis);
		vnull
	)

	let flush = vifun0 (fun vthis ->
		flush (this vthis);
		vnull
	)

	let seek = vifun2 (fun vthis pos mode ->
		let this = this vthis in
		let pos = decode_int pos in
		let mode,_ = decode_enum mode in
		seek_out this (match mode with 0 -> pos | 1 -> pos_out this + pos | 2 -> out_channel_length this + pos | _ -> die "" __LOC__);
		vnull
	)

	let tell = vifun0 (fun vthis ->
		vint (pos_out (this vthis))
	)

	let writeByte = vifun1 (fun vthis c ->
		output_char (this vthis) (char_of_int (decode_int c));
		vnull
	)

	let writeBytes = vifun3 (fun vthis bytes pos len ->
		let this = this vthis in
		let bytes = decode_bytes bytes in
		let pos = decode_int pos in
		let len = decode_int len in
		output this bytes pos len;
		vint len
	)
end

module StdFPHelper = struct
	let doubleToI64 = vfun1 (fun v ->
		let f = num v in
		let i64 = Int64.bits_of_float f in
		EvalIntegers.encode_haxe_i64_direct i64
	)

	let floatToI32 = vfun1 (fun f ->
		let f = num f in
		let i32 = Int32.bits_of_float f in
		vint32 i32
	)

	let i32ToFloat = vfun1 (fun i ->
		let i32 = decode_i32 i in
		let f = Int32.float_of_bits i32 in
		vfloat f
	)

	let i64ToDouble = vfun2 (fun low high ->
		let low = decode_i32 low in
		let high = decode_i32 high in
		let b = Bytes.make 8 '0' in
		EvalBytes.write_i32 b 0 low;
		EvalBytes.write_i32 b 4 high;
		let i64 = EvalBytes.read_i64 b 0 in
		vfloat (Int64.float_of_bits i64)
	)
end

module StdFileSystem = struct
	let rec remove_trailing_slash p =
		let l = String.length p in
		if l = 0 then
			"" (* don't be retarded *)
		else match p.[l-1] with
			| '\\' | '/' -> remove_trailing_slash (String.sub p 0 (l - 1))
			| _ -> p

	let patch_path s =
		if String.length s > 1 && String.length s <= 3 && s.[1] = ':' then Path.add_trailing_slash s
		else if s = "/" then "/"
		else remove_trailing_slash s

	let createDirectory = vfun1 (fun path ->
		catch_unix_error Path.mkdir_from_path_unix_err (Path.add_trailing_slash (decode_string path));
		vnull
	)

	let deleteDirectory = vfun1 (fun path ->
		catch_unix_error Unix.rmdir (decode_string path);
		vnull
	)

	let deleteFile = vfun1 (fun path ->
		(try Sys.remove (decode_string path) with Sys_error s -> exc_string s);
		vnull
	)

	let exists = vfun1 (fun path ->
		let b = try Sys.file_exists (patch_path (decode_string path)) with Sys_error _ -> false in
		vbool b
	)

	let fullPath = vfun1 (fun relPath ->
		try create_unknown (Extc.get_full_path (decode_string relPath)) with exc -> exc_string (Printexc.to_string exc)
	)

	let isDirectory = vfun1 (fun dir ->
		let b = try Sys.is_directory (patch_path(decode_string dir)) with Sys_error _ -> false in
		vbool b
	)

	let readDirectory = vfun1 (fun dir ->
		let dir = decode_string dir in
		let d = try
			if not (Sys.is_directory (patch_path dir)) then exc_string "No such directory";
			Sys.readdir dir
		with Sys_error s ->
			exc_string s
		in
		encode_array (Array.to_list (Array.map (fun s -> create_unknown s) d))
	)

	let rename = vfun2 (fun path newPath ->
		(try Sys.rename (decode_string path) (decode_string newPath) with Sys_error s -> exc_string s);
		vnull
	)

	let stat = vfun1 (fun path ->
		let s = catch_unix_error Unix.stat (patch_path (decode_string path)) in
		encode_obj [
			key_gid,vint s.st_gid;
			key_uid,vint s.st_uid;
			key_atime,StdDate.encode_date s.st_atime;
			key_mtime,StdDate.encode_date s.st_mtime;
			key_ctime,StdDate.encode_date s.st_ctime;
			key_dev,vint s.st_dev;
			key_ino,vint s.st_ino;
			key_nlink,vint s.st_nlink;
			key_rdev,vint s.st_rdev;
			key_size,vint s.st_size;
			key_mode,vint s.st_perm;
		]
	)
end

module StdGc = struct
	open Gc
	let key_minor_heap_size = hash "minor_heap_size"
	let key_major_heap_increment = hash "major_heap_increment"
	let key_space_overhead = hash "space_overhead"
	let key_verbose = hash "verbose"
	let key_max_overhead = hash "max_overhead"
	let key_stack_limit = hash "stack_limit"
	let key_allocation_policy = hash "allocation_policy"
	let key_minor_words = hash "minor_words"
	let key_minor_words = hash "minor_words"
	let key_promoted_words = hash "promoted_words"
	let key_major_words = hash "major_words"
	let key_minor_collections = hash "minor_collections"
	let key_major_collections = hash "major_collections"
	let key_heap_words = hash "heap_words"
	let key_heap_chunks = hash "heap_chunks"
	let key_live_words = hash "live_words"
	let key_live_blocks = hash "live_blocks"
	let key_free_words = hash "free_words"
	let key_free_blocks = hash "free_blocks"
	let key_largest_free = hash "largest_free"
	let key_fragments = hash "fragments"
	let key_compactions = hash "compactions"
	let key_top_heap_words = hash "top_heap_words"
	let key_stack_size = hash "stack_size"

	let encode_stats stats =
		encode_obj [
			key_minor_words,vfloat stats.minor_words;
			key_promoted_words,vfloat stats.promoted_words;
			key_major_words,vfloat stats.major_words;
			key_minor_collections,vint stats.minor_collections;
			key_major_collections,vint stats.major_collections;
			key_heap_words,vint stats.heap_words;
			key_heap_chunks,vint stats.heap_chunks;
			key_live_words,vint stats.live_words;
			key_live_blocks,vint stats.live_blocks;
			key_free_words,vint stats.free_words;
			key_free_blocks,vint stats.free_blocks;
			key_largest_free,vint stats.largest_free;
			key_fragments,vint stats.fragments;
			key_compactions,vint stats.compactions;
			key_top_heap_words,vint stats.top_heap_words;
			key_stack_size,vint stats.stack_size;
		]

	let allocated_bytes = vfun0 (fun () -> vfloat (Gc.allocated_bytes()))

	let compact = vfun0 (fun () -> Gc.compact(); vnull )

	let counters = vfun0 (fun () ->
		let (minor_words,promoted_words,major_words) = Gc.counters() in
		encode_obj [
			key_minor_words,vfloat minor_words;
			key_promoted_words,vfloat promoted_words;
			key_major_words,vfloat major_words;
		]
	)

	let finalise = vfun2 (fun f v ->
		let f = fun v ->
			ignore(call_value f [v])
		in
		Gc.finalise f v;
		vnull
	)

	let finalise_release = vfun0 (fun () ->
		Gc.finalise_release();
		vnull
	)

	let full_major = vfun0 (fun () -> Gc.full_major(); vnull )

	let get = vfun0 (fun () ->
		let control = Gc.get() in
		encode_obj [
			key_minor_heap_size,vint control.minor_heap_size;
			key_major_heap_increment,vint control.major_heap_increment;
			key_space_overhead,vint control.space_overhead;
			key_verbose,vint control.verbose;
			key_max_overhead,vint control.max_overhead;
			key_stack_limit,vint control.stack_limit;
			key_allocation_policy,vint control.allocation_policy;
		]
	)

	let major = vfun0 (fun () -> Gc.major(); vnull )

	let major_slice = vfun1 (fun n -> vint (Gc.major_slice (decode_int n)))

	let minor = vfun0 (fun () -> Gc.minor(); vnull )

	let print_stat = vfun1 (fun out_channel ->
		let out_channel = match out_channel with
			| VInstance {ikind = IOutChannel ch} -> ch
			| _ -> unexpected_value out_channel "Output"
		in
		Gc.print_stat out_channel;
		vnull
	)

	let quick_stat = vfun0 (fun () -> encode_stats (Gc.quick_stat()))

	let set = vfun1 (fun r ->
		let r = decode_object r in
		let field key = decode_int (object_field r key) in
		let control = { (Gc.get()) with
			minor_heap_size = field key_minor_heap_size;
			major_heap_increment = field key_major_heap_increment;
			space_overhead = field key_space_overhead;
			verbose = field key_verbose;
			max_overhead = field key_max_overhead;
			stack_limit = field key_stack_limit;
		} in
		(* Awkward hack to avoid warning. *)
		let control = {control with allocation_policy = field key_allocation_policy} in
		Gc.set control;
		vnull
	)

	let stat = vfun0 (fun () -> encode_stats (Gc.stat()))
end

module StdHost = struct
	let int32_addr h =
		let base = Int32.to_int (Int32.logand h 0xFFFFFFl) in
		let str = Printf.sprintf "%ld.%d.%d.%d" (Int32.shift_right_logical h 24) (base lsr 16) ((base lsr 8) land 0xFF) (base land 0xFF) in
		catch_unix_error Unix.inet_addr_of_string str

	let localhost = vfun0 (fun () ->
		create_unknown (catch_unix_error Unix.gethostname())
	)

	let hostReverse = vfun1 (fun ip ->
		let ip = decode_i32 ip in
		try create_unknown (catch_unix_error Unix.gethostbyaddr (int32_addr ip)).h_name with Not_found -> exc_string "Could not reverse host"
	)

	let hostToString = vfun1 (fun ip ->
		let ip = decode_i32 ip in
		create_unknown (catch_unix_error Unix.string_of_inet_addr (int32_addr ip))
	)

	let resolve = vfun1 (fun name ->
		let name = decode_string name in
		let h = try Unix.gethostbyname name with Not_found -> exc_string (Printf.sprintf "Could not resolve host %s" name) in
		let addr = catch_unix_error Unix.string_of_inet_addr h.h_addr_list.(0) in
		let a, b, c, d = Scanf.sscanf addr "%d.%d.%d.%d" (fun a b c d -> a,b,c,d) in
		vint32 (Int32.logor (Int32.shift_left (Int32.of_int a) 24) (Int32.of_int (d lor (c lsl 8) lor (b lsl 16))))
	)
end

module StdLock = struct
	let this vthis = match vthis with
		| VInstance {ikind = ILock lock} -> lock
		| v -> unexpected_value v "Lock"

	let release = vifun0 (fun vthis ->
		let this = this vthis in
		Deque.push this.ldeque vnull;
		vnull
	)

	let wait = vifun1 (fun vthis timeout ->
		let lock = this vthis in
		let rec loop target_time =
			match Deque.pop lock.ldeque false with
			| None ->
				if Sys.time() >= target_time then
					vfalse
				else begin
					Thread.yield();
					loop target_time
				end
			| Some _ ->
				vtrue
		in
		match Deque.pop lock.ldeque false with
		| None ->
			begin match timeout with
				| VNull ->
					ignore(Deque.pop lock.ldeque true);
					vtrue
				| _ ->
					let target_time = (Sys.time()) +. num timeout in
					loop target_time
			end
		| Some _ ->
			vtrue
	)
end

let lineEnd = match Sys.os_type with
	| "Win32" | "Cygwin" -> "\r\n"
	| _ -> "\n"

module StdLog = struct
	let key_fileName = hash "fileName"
	let key_lineNumber = hash "lineNumber"
	let key_customParams = hash "customParams"

	let trace = vfun2 (fun v infos ->
		let s = value_string v in
		let s = match infos with
			| VNull -> (Printf.sprintf "%s" s) ^ lineEnd
			| _ ->  let infos = decode_object infos in
				let file_name = decode_string (object_field infos key_fileName) in
				let line_number = decode_int (object_field infos key_lineNumber) in
				let l = match object_field infos key_customParams with
					| VArray va -> s :: (List.map value_string (EvalArray.to_list va))
					| _ -> [s]
				in
				(Printf.sprintf "%s:%i: %s" file_name line_number (String.concat "," l)) ^ lineEnd in
		((get_ctx()).curapi.MacroApi.get_com()).Common.print s;
		vnull
	)
end

let encode_list_iterator l =
	let l = ref l in
	encode_obj [
		key_hasNext,vifun0 (fun _ ->
			match !l with [] -> vfalse | _ -> vtrue
		);
		key_next,vifun0 (fun _ -> match !l with
			| [] -> vnull
			| v :: l' -> l := l'; v
		)
	]

let map_key_value_iterator path = vifun0 (fun vthis ->
	let ctx = get_ctx() in
	let vit = encode_instance path in
	let fnew = get_instance_constructor ctx path null_pos in
	ignore(call_value_on vit (Lazy.force fnew) [vthis]);
	vit
)

module StdIntMap = struct
	let this vthis = match vthis with
		| VInstance {ikind = IIntMap h} -> h
		| v -> unexpected_value v "int map"

	let copy = vifun0 (fun vthis ->
		let copied = IntHashtbl.copy (this vthis) in
		encode_int_map_direct copied
	)

	let exists = vifun1 (fun vthis vkey ->
		vbool (IntHashtbl.mem (this vthis) (decode_int vkey))
	)

	let get = vifun1 (fun vthis vkey ->
		try IntHashtbl.find (this vthis) (decode_int vkey)
		with Not_found -> vnull
	)

	let iterator = vifun0 (fun vthis ->
		let keys = IntHashtbl.fold (fun _ v acc -> v :: acc) (this vthis) [] in
		encode_list_iterator keys
	)

	let keys = vifun0 (fun vthis ->
		let keys = IntHashtbl.fold (fun k _ acc -> vint k :: acc) (this vthis) [] in
		encode_list_iterator keys
	)

	let keyValueIterator = map_key_value_iterator key_haxe_iterators_map_key_value_iterator

	let remove = vifun1 (fun vthis vkey ->
		let this = this vthis in
		let key = decode_int vkey in
		let b = IntHashtbl.mem this key in
		IntHashtbl.remove this key;
		vbool b
	)

	let set = vifun2 (fun vthis vkey vvalue ->
		IntHashtbl.add (this vthis) (decode_int vkey) vvalue;
		vnull
	)

	let toString = vifun0 (fun vthis ->
		let this = this vthis in
		let l = IntHashtbl.fold (fun key vvalue acc ->
			(join empty_string [create_ascii (string_of_int key); create_ascii " => "; s_value 0 vvalue]) :: acc) this [] in
		let s = join rcomma l in
		let s = join empty_string [rbkopen;s;rbkclose] in
		vstring s
	)

	let clear = vifun0 (fun vthis ->
		IntHashtbl.clear (this vthis);
		vnull
	)
end

module StdStringMap = struct
	let this vthis = match vthis with
		| VInstance {ikind = IStringMap h} -> h
		| v -> unexpected_value v "string map"

	let copy = vifun0 (fun vthis ->
		let copied = StringHashtbl.copy (this vthis) in
		encode_string_map_direct copied
	)

	let exists = vifun1 (fun vthis vkey ->
		vbool (StringHashtbl.mem (this vthis) (decode_vstring vkey))
	)

	let get = vifun1 (fun vthis vkey ->
		try snd (StringHashtbl.find (this vthis) (decode_vstring vkey))
		with Not_found -> vnull
	)

	let iterator = vifun0 (fun vthis ->
		let keys = StringHashtbl.fold (fun _ (_,v) acc -> v :: acc) (this vthis) [] in
		encode_list_iterator keys
	)

	let keys = vifun0 (fun vthis ->
		let keys = StringHashtbl.fold (fun _ (k,_) acc -> vstring k :: acc) (this vthis) [] in
		encode_list_iterator keys
	)

	let keyValueIterator = map_key_value_iterator key_haxe_iterators_map_key_value_iterator

	let remove = vifun1 (fun vthis vkey ->
		let this = this vthis in
		let key = decode_vstring vkey in
		let b = StringHashtbl.mem this key in
		StringHashtbl.remove this key;
		vbool b
	)

	let set = vifun2 (fun vthis vkey vvalue ->
		StringHashtbl.add (this vthis) (decode_vstring vkey) vvalue;
		vnull
	)

	let toString = vifun0 (fun vthis ->
		let this = this vthis in
		let l = StringHashtbl.fold (fun _ (key,vvalue) acc ->
			(join empty_string [key; create_ascii " => "; s_value 0 vvalue]) :: acc) this [] in
		let s = join rcomma l in
		let s = join empty_string [rbkopen;s;rbkclose] in
		vstring s
	)

	let clear = vifun0 (fun vthis ->
		StringHashtbl.clear (this vthis);
		vnull
	)
end

module StdObjectMap = struct
	let this vthis = match vthis with
		| VInstance {ikind = IObjectMap h} -> Obj.magic h
		| v -> unexpected_value v "int map"

	let copy = vifun0 (fun vthis ->
		let copied = ValueHashtbl.copy (this vthis) in
		encode_object_map_direct copied
	)

	let exists = vifun1 (fun vthis vkey ->
		vbool (ValueHashtbl.mem (this vthis) vkey)
	)

	let get = vifun1 (fun vthis vkey ->
		try ValueHashtbl.find (this vthis) vkey
		with Not_found -> vnull
	)

	let iterator = vifun0 (fun vthis ->
		let keys = ValueHashtbl.fold (fun _ v acc -> v :: acc) (this vthis) [] in
		encode_list_iterator keys
	)

	let keys = vifun0 (fun vthis ->
		let keys = ValueHashtbl.fold (fun k _ acc -> k :: acc) (this vthis) [] in
		encode_list_iterator keys
	)

	let keyValueIterator = map_key_value_iterator key_haxe_iterators_map_key_value_iterator

	let remove = vifun1 (fun vthis vkey ->
		let this = this vthis in
		let b = ValueHashtbl.mem this vkey in
		ValueHashtbl.remove this vkey;
		vbool b
	)

	let set = vifun2 (fun vthis vkey vvalue ->
		ValueHashtbl.replace (this vthis) vkey vvalue;
		vnull
	)

	let toString = vifun0 (fun vthis ->
		let this = this vthis in
		let l = ValueHashtbl.fold (fun key vvalue acc ->
			(join empty_string [s_value 0 key; create_ascii " => "; s_value 0 vvalue]) :: acc) this [] in
		let s = join rcomma l in
		let s = join empty_string [rbkopen;s;rbkclose] in
		vstring s
	)

	let clear = vifun0 (fun vthis ->
		ValueHashtbl.reset (this vthis);
		vnull
	)
end

let random = Random.State.make_self_init()

module StdMath = struct
	let to_int f = Int32.of_float (mod_float f 2147483648.0)

	let nan = vfloat nan
	let negative_infinity = vfloat neg_infinity
	let pi = vfloat (4.0 *. atan 1.0)
	let positive_infinity = vfloat infinity

	let abs = vfun1 (fun v ->
		match v with
		| VInt32 i -> vint32 (Int32.abs i)
		| VFloat f -> vfloat (abs_float f)
		| _ -> unexpected_value v "number"
	)

	let acos = vfun1 (fun v -> vfloat (acos (num v)))
	let asin = vfun1 (fun v -> vfloat (asin (num v)))
	let atan = vfun1 (fun v -> vfloat (atan (num v)))
	let atan2 = vfun2 (fun a b -> vfloat (atan2 (num a) (num b)))
	let ceil = vfun1 (fun v -> match v with VInt32 _ -> v | _ -> vint32 (to_int (ceil (num v))))
	let cos = vfun1 (fun v -> vfloat (cos (num v)))
	let exp = vfun1 (fun v -> vfloat (exp (num v)))
	let fceil = vfun1 (fun v -> vfloat (Stdlib.ceil (num v)))
	let ffloor = vfun1 (fun v -> vfloat (Stdlib.floor (num v)))
	let floor = vfun1 (fun v -> match v with VInt32 _ -> v | _ -> vint32 (to_int (floor (num v))))
	let fround = vfun1 (fun v -> vfloat (Stdlib.floor (num v +. 0.5)))
	let isFinite = vfun1 (fun v -> vbool (match v with VFloat f -> f <> infinity && f <> neg_infinity && f = f | _ -> true))
	let isNaN = vfun1 (fun v -> vbool (match v with VFloat f -> f <> f | VInt32 _ -> false | _ -> true))
	let log = vfun1 (fun v -> vfloat (Stdlib.log (num v)))

	let max = vfun2 (fun a b ->
		let a = num a in
		let b = num b in
		vfloat (if a < b then b else if b <> b then b else a);
	)

	let min = vfun2 (fun a b ->
		let a = num a in
		let b = num b in
		vfloat (if a < b then a else if a <> a then a else b);
	)

	let pow = vfun2 (fun a b -> vfloat ((num a) ** (num b)))
	let random = vfun0 (fun () -> vfloat (Random.State.float random 1.))
	let round = vfun1 (fun v -> match v with VInt32 _ -> v | _ -> vint32 (to_int (Stdlib.floor (num v +. 0.5))))
	let sin = vfun1 (fun v -> vfloat (sin (num v)))

	let sqrt = vfun1 (fun v ->
		let v = num v in
		if v < 0. then nan else vfloat (sqrt v)
	)

	let tan = vfun1 (fun v -> vfloat (tan (num v)))
end

module StdMd5 = struct
	let encode = vfun1 (fun s ->
		let s = decode_string s in
		encode_string (Digest.to_hex (Digest.string s))
	)

	let make = vfun1 (fun b ->
		let b = decode_bytes b in
		encode_bytes (Bytes.unsafe_of_string (Digest.string (Bytes.unsafe_to_string b)))
	)
end

module StdMutex = struct
	let this vthis = match vthis with
		| VInstance {ikind=IMutex mutex} -> mutex
		| _ -> unexpected_value vthis "Mutex"

	let acquire = vifun0 (fun vthis ->
		let mutex = this vthis in
		let thread_id = Thread.id (Thread.self()) in
		(match mutex.mowner with
		| None ->
			Mutex.lock mutex.mmutex;
			mutex.mowner <- Some (thread_id,1)
		| Some (id,n) ->
			if id = thread_id then
				mutex.mowner <- Some (thread_id,n + 1)
			else begin
				Mutex.lock mutex.mmutex;
				mutex.mowner <- Some (thread_id,1)
			end
		);
		vnull
	)

	let release = vifun0 (fun vthis ->
		let mutex = this vthis in
		(match mutex.mowner with
		| Some (id,n) when n > 1 ->
			mutex.mowner <- Some (id,n - 1)
		| _ ->
			mutex.mowner <- None;
			Mutex.unlock mutex.mmutex;
		);
		vnull
	)

	let tryAcquire = vifun0 (fun vthis ->
		let mutex = this vthis in
		let thread_id = Thread.id (Thread.self()) in
		match mutex.mowner with
		| Some (id,n) when id = thread_id ->
			mutex.mowner <- Some (thread_id,n + 1);
			vtrue
		| _ ->
			if Mutex.try_lock mutex.mmutex then begin
				mutex.mowner <- Some (thread_id,1);
				vtrue
			end else
				vfalse
	)
end

module StdNativeProcess = struct

	let this vthis = match vthis with
		| VInstance {ikind=IProcess proc} -> proc
		| _ -> unexpected_value vthis "NativeProcess"

	let call f vthis bytes pos len =
		let this = this vthis in
		let bytes = decode_bytes bytes in
		let pos = decode_int pos in
		let len = decode_int len in
		f this (Bytes.unsafe_to_string bytes) pos len

	let process_catch f vthis =
		try f (this vthis)
		with Failure msg -> exc_string msg

	let close = vifun0 (fun vthis ->
		process_catch Process.close vthis;
		vnull
	)

	let exitCode = vifun0 (fun vthis ->
		vint (process_catch Process.exit vthis)
	)

	let getPid = vifun0 (fun vthis ->
		vint (process_catch Process.pid vthis)
	)

	let kill = vifun0 (fun vthis ->
		process_catch Process.kill vthis;
		vnull
	)

	let readStderr = vifun3 (fun vthis bytes pos len ->
		try vint (call Process.read_stderr vthis bytes pos len) with _ -> exc_string "Could not read stderr"
	)

	let readStdout = vifun3 (fun vthis bytes pos len ->
		try vint (call Process.read_stdout vthis bytes pos len) with _ -> exc_string "Could not read stdout"
	)

	let closeStdin = vifun0 (fun vthis ->
		process_catch Process.close_stdin vthis;
		vnull
	)

	let writeStdin = vifun3 (fun vthis bytes pos len ->
		vint (call Process.write_stdin vthis bytes pos len)
	)
end

module StdReflect = struct

	let r_get_ = create_ascii "get_"
	let r_set_ = create_ascii "set_"

	let callMethod = vfun3 (fun o f args ->
		call_value_on o f (decode_array args)
	)

	let compare = vfun2 (fun a b ->
		vint (match compare a b with
		| CEq -> 0
		| CInf -> -1
		| CSup -> 1
		| CUndef -> -1)
	)

	let compareMethods = vfun2 (fun a b ->
		let loop a b = a == b || match a,b with
			| VFunction(f1,_),VFunction(f2,_) -> f1 == f2
			| VFieldClosure(v1,f1),VFieldClosure(v2,f2) -> f1 == f2 && EvalMisc.compare v1 v2 = CEq
			| _ -> false
		in
		vbool (loop a b)
	)

	let copy = vfun1 (fun o -> match vresolve o with
		| VNull -> VNull
		| VObject o -> VObject { o with ofields = Array.copy o.ofields }
		| VInstance vi -> vinstance {
			ifields = Array.copy vi.ifields;
			iproto = vi.iproto;
			ikind = vi.ikind;
		}
		| VString _ -> o
		| VArray va -> VArray { va with avalues = Array.copy va.avalues }
		| VVector vv -> VVector (Array.copy vv)
		| _ -> unexpected_value o "object"
	)

	let deleteField = vfun2 (fun o name ->
		let name = hash (decode_vstring name).sstring in
		match vresolve o with
		| VObject o ->
			begin match o.oproto with
			| OProto proto ->
				let found = ref false in
				let fields = IntMap.fold (fun name' i acc ->
					if name = name' then begin
						found := true;
						acc
					end else
						(name',o.ofields.(i)) :: acc
				) proto.pinstance_names [] in
				if !found then begin
					update_object_prototype o fields;
					vtrue
				end else
					vfalse
			| ODictionary d ->
				let has = IntMap.mem name d in
				if has then o.oproto <- ODictionary (IntMap.remove name d);
				vbool has
			end
		| _ ->
			vfalse
	)

	let field' = vfun2 (fun o name ->
		if o = vnull then vnull else dynamic_field o (hash (decode_vstring name).sstring)
	)

	let fields = vfun1 (fun o ->
		let proto_fields proto = IntMap.fold (fun name _ acc -> name :: acc) proto.pnames [] in
		let fields = match vresolve o with
			| VObject o -> List.map fst (object_fields o)
			| VInstance vi -> IntMap.fold (fun name _ acc -> name :: acc) vi.iproto.pinstance_names []
			| VPrototype proto -> proto_fields proto
			| VNull -> []
			| VString _ | VArray _ | VVector _ -> [key_length]
			| _ -> unexpected_value o "object"
		in
		encode_array (List.map (fun i -> encode_string (rev_hash i)) fields)
	)

	let getProperty = vfun2 (fun o name ->
		if o = VNull then
			vnull
		else begin
			let name = decode_vstring name in
			let name_get = hash (concat r_get_ name).sstring in
			let vget = field o name_get in
			if vget <> VNull then call_value_on o vget []
			else dynamic_field o (hash name.sstring)
		end
	)

	let hasField = vfun2 (fun o field ->
		let name = hash (decode_vstring field).sstring in
		let b = match vresolve o with
			| VObject o ->
				begin match o.oproto with
				| OProto proto -> IntMap.mem name proto.pinstance_names
				| ODictionary d -> IntMap.mem name d
				end
			| VInstance vi -> IntMap.mem name vi.iproto.pinstance_names || IntMap.mem name vi.iproto.pnames
			| VPrototype proto -> IntMap.mem name proto.pnames
			| _ -> false (* issue #10993 *)
		in
		vbool b
	)

	let isEnumValue = vfun1 (fun v -> match v with
		| VEnumValue _ -> vtrue
		| _ -> vfalse
	)

	let isFunction = vfun1 (fun f ->
		match f with
		| VFunction _ | VFieldClosure _ -> vtrue
		| _ -> vfalse
	)

	let isObject = vfun1 (fun v -> match vresolve v with
		| VObject _ | VString _ | VArray _ | VVector _ | VInstance _ | VPrototype _ -> vtrue
		| _ -> vfalse
	)

	let makeVarArgs = vfun1 (fun f ->
		vstatic_function ((fun vl -> call_value f [encode_array vl]))
	)

	let setField = vfun3 (fun o name v ->
		(try set_field_runtime o (hash (decode_vstring name).sstring) v with Not_found -> ()); vnull
	)

	let setProperty = vfun3 (fun o name v ->
		let name = decode_vstring name in
		let name_set = hash (concat r_set_ name).sstring in
		let vset = field o name_set in
		if vset <> VNull then call_value_on o vset [v]
		else begin
			(try set_field_runtime o (hash name.sstring) v with Not_found -> ());
			vnull
		end
	)
end

module StdResource = struct
	open Common

	let listNames = vfun0 (fun () ->
		encode_array (List.map create_unknown (hashtbl_keys ((get_ctx()).curapi.MacroApi.get_com()).resources))
	)

	let getString = vfun1 (fun name ->
		try ((create_unknown (Hashtbl.find ((get_ctx()).curapi.MacroApi.get_com()).resources (decode_string name)))) with Not_found -> vnull
	)

	let getBytes = vfun1 (fun name ->
		try encode_bytes (Bytes.unsafe_of_string (Hashtbl.find ((get_ctx()).curapi.MacroApi.get_com()).resources (decode_string name))) with Not_found -> vnull
	)
end

module StdSha1 = struct
	let encode = vfun1 (fun s ->
		let s = decode_string s in
		encode_string (Sha1.to_hex (Sha1.string s))
	)

	let make = vfun1 (fun b ->
		let b = decode_bytes b in
		encode_bytes (Bytes.unsafe_of_string (Sha1.to_bin (Sha1.string (Bytes.unsafe_to_string b))))
	)
end

module StdSocket = struct
	let inet_addr_to_int32 addr =
		let s = catch_unix_error Unix.string_of_inet_addr addr in
		match List.map Int32.of_string (ExtString.String.nsplit s ".") with
			| [a;b;c;d] -> Int32.add (Int32.add (Int32.add (Int32.shift_left a 24) (Int32.shift_left b 16)) (Int32.shift_left c 8)) d
			| _ -> die "" __LOC__

	let this vthis = match vthis with
		| VInstance {ikind = ISocket sock} -> sock
		| _ -> unexpected_value vthis "NativeSocket"

	let accept = vifun0 (fun vthis ->
		let this = this vthis in
		let socket,_ = catch_unix_error Unix.accept this in
		encode_instance key_eval_vm_NativeSocket ~kind:(ISocket socket)
	)

	let bind = vifun2 (fun vthis host port ->
		let this = this vthis in
		let host = decode_i32 host in
		let port = decode_int port in
		catch_unix_error Unix.bind this (ADDR_INET (StdHost.int32_addr host,port));
		vnull
	)

	let close = vifun0 (fun vthis ->
		catch_unix_error Unix.close (this vthis);
		vnull
	)

	let connect = vifun2 (fun vthis host port ->
		let this = this vthis in
		let host = decode_i32 host in
		let port = decode_int port in
		catch_unix_error (Unix.connect this) (ADDR_INET (StdHost.int32_addr host,port));
		vnull
	)

	let host = vifun0 (fun vthis ->
		match catch_unix_error Unix.getsockname (this vthis) with
		| ADDR_INET (addr,port) ->
			encode_obj [
				key_ip,vint32 (inet_addr_to_int32 addr);
				key_port,vint port;
			]
		| _ -> die "" __LOC__
	)

	let listen = vifun1 (fun vthis connections ->
		let this = this vthis in
		let connections = decode_int connections in
		catch_unix_error Unix.listen this connections;
		vnull
	)

	let peer = vifun0 (fun vthis ->
		match catch_unix_error Unix.getpeername (this vthis) with
		| ADDR_INET (addr,port) ->
			encode_obj [
				key_ip,vint32 (inet_addr_to_int32 addr);
				key_port,vint port;
			]
		| _ -> die "" __LOC__
	)

	let receive = vifun3 (fun vthis buf pos len ->
		let this = this vthis in
		let buf = decode_bytes buf in
		let pos = decode_int pos in
		let len = decode_int len in
		vint (catch_unix_error Unix.recv this buf pos len [])
	)

	let receiveChar = vifun0 (fun vthis ->
		let buf = Bytes.make 1 '\000' in
		ignore(catch_unix_error Unix.recv (this vthis) buf 0 1 []);
		vint (int_of_char (Bytes.unsafe_get buf 0))
	)

	let select = vfun4 (fun read write others timeout ->
		let proto = get_instance_prototype (get_ctx()) key_sys_net_Socket null_pos in
		let i = get_instance_field_index proto key_socket null_pos in
		let pair = function
			| VInstance vi as v -> this vi.ifields.(i),v
			| v -> unexpected_value v "NativeSocket"
		in
		let decode_optional_array = function
			| VNull -> []
			| VArray va -> EvalArray.to_list va
			| v -> unexpected_value v "array"
		in
		let read = List.map pair (decode_optional_array read) in
		let write = List.map pair (decode_optional_array write) in
		let others = List.map pair (decode_optional_array others) in
		let timeout = match timeout with VNull -> 0. | VInt32 i -> Int32.to_float i | VFloat f -> f | _ -> unexpected_value timeout "number" in
		let read',write',others' = catch_unix_error Unix.select (List.map fst read) (List.map fst write) (List.map fst others) timeout in
		let read = List.map (fun sock -> List.assq sock read) read' in
		let write = List.map (fun sock -> List.assq sock write) write' in
		let others = List.map (fun sock -> List.assq sock others) others' in
		encode_obj [
			key_read,encode_array read;
			key_write,encode_array write;
			key_others,encode_array others;
		]
	)

	let send = vifun3 (fun vthis buf pos len ->
		let this = this vthis in
		let buf = decode_bytes buf in
		let pos = decode_int pos in
		let len = decode_int len in
		vint (catch_unix_error Unix.send this buf pos len [])
	)

	let sendChar = vifun1 (fun vthis char ->
		let this = this vthis in
		let char = decode_int char in
		ignore(catch_unix_error Unix.send this (Bytes.make 1 (char_of_int char)) 0 1 []);
		VNull
	)

	let setFastSend = vifun1 (fun vthis b ->
		let this = this vthis in
		let b = decode_bool b in
		catch_unix_error Unix.setsockopt this TCP_NODELAY b;
		vnull
	)

	let setBroadcast = vifun1 (fun vthis b ->
		let this = this vthis in
		let b = decode_bool b in
		catch_unix_error Unix.setsockopt this SO_BROADCAST b;
		vnull
	)

	let setTimeout = vifun1 (fun vthis timeout ->
		let this = this vthis in
		let timeout = match timeout with VNull -> 0. | VInt32 i -> Int32.to_float i | VFloat f -> f | _ -> unexpected_value timeout "number" in
		let timeout = timeout *. 1000. in
		catch_unix_error (fun () ->
			Unix.setsockopt_float this SO_RCVTIMEO timeout;
			Unix.setsockopt_float this SO_SNDTIMEO timeout;
		) ();
		vnull
	)

	let shutdown = vifun2 (fun vthis read write ->
		let this = this vthis in
		let mode = match read,write with
			| VTrue,VTrue -> Unix.SHUTDOWN_ALL
			| VTrue,_ -> SHUTDOWN_RECEIVE
			| _,VTrue -> SHUTDOWN_SEND
			| _ -> exc_string "Nothing to shut down"
		in
		catch_unix_error Unix.shutdown this mode;
		vnull
	)
end

module StdStd = struct
	let isOfType = vfun2 (fun v t -> match t with
		| VNull -> vfalse
		| VPrototype proto -> vbool (is v proto.ppath)
		| _ -> vfalse
	)

	let is' = isOfType

	let downcast = vfun2 (fun v t -> match t with
		| VPrototype proto ->
			if is v proto.ppath then v else vnull
		| _ -> vfalse
	)

	let instance = downcast

	let string = vfun1 (fun v -> match v with
		| VString _ -> v
		| _ -> vstring (s_value 0 v)
	)

	let int = vfun1 (fun v ->
		try vint (int_of_float (num v)) with _ -> vnull
	)

	let parseInt = vfun1 (fun v ->
		try vint32 (Numeric.parse_int (decode_string v)) with _ -> vnull
	)

	let parseFloat = vfun1 (fun v ->
		try vfloat (Numeric.parse_float (decode_string v)) with _ -> vfloat nan
	)

	let random = vfun1 (fun v ->
		let v = decode_i32 v in
		vint32 (Random.State.int32 random (if v <= Int32.zero then Int32.one else v))
	);
end

module StdString = struct
	let this vthis = match vthis with
		| VString s -> s
		| v -> unexpected_value v "string"

	let charAt = vifun1 (fun vthis index ->
		let this = this vthis in
		let i = decode_int index in
		if i < 0 || i >= this.slength then v_empty_string
		else vstring (from_char_code (char_at this i))
	)

	let charCodeAt = vifun1 (fun vthis index ->
		let this = this vthis in
		let i = decode_int index in
		if i < 0 || i >= this.slength then vnull
		else vint (char_at this i)
	)

	let fromCharCode = vfun1 (fun i ->
		let i = decode_int i in
		try
			vstring (from_char_code i)
		with
		| Not_found ->
			vnull
	)

	let indexOf = vifun2 (fun vthis str startIndex ->
		let str = this str in
		let this = this vthis in
		let i = default_int startIndex 0 in
		let i = max 0 i in
		try
			if str.slength = 0 then
				vint (min i this.slength)
			else begin
				let i =
					if i >= this.slength then raise Not_found
					else if i < 0 then max (this.slength + i) 0
					else i
				in
				let b = get_offset this i in
				let offset,_,_ = find_substring this str false i b in
				vint offset
			end
		with Not_found ->
			vint (-1)
	)

	let lastIndexOf = vifun2 (fun vthis str startIndex ->
		let str = this str in
		let this = this vthis in
		try
			if str.slength = 0 then begin
				let i = default_int startIndex this.slength in
				vint (max 0 (min i this.slength))
			end else begin
				let i = default_int startIndex (this.slength - str.slength) in
				let i = if i < 0 then raise Not_found else if i >= this.slength - str.slength then this.slength - str.slength else i in
				let b = get_offset this i in
				let offset,_,_ = find_substring this str true i b in
				vint offset
			end
		with Not_found ->
			vint (-1)
	)

	let split = vifun1 (fun vthis delimiter ->
		let this = this vthis in
		let s = this.sstring in
		let delimiter = decode_vstring delimiter in
		let bl_delimiter = String.length delimiter.sstring in
		let bl_this = String.length s in
		let encode_range pos length clength =
			let s = String.sub s pos length in
			vstring (create_with_length s clength)
		in
		if bl_delimiter = 0 then begin
			let acc = DynArray.create () in
			UTF8.iter (fun uc ->
				DynArray.add acc (vstring (create_with_length (UTF8.init 1 (fun _ -> uc)) 1));
			) s;
			encode_array (DynArray.to_list acc)
		end else if bl_delimiter > bl_this then
			encode_array [encode_range 0 bl_this this.slength]
		else begin
			let acc = DynArray.create () in
			let f = find_substring this delimiter false in
			let rec loop c_index b_index =
				try
					let c_offset,b_offset,next = f c_index b_index in
					DynArray.add acc (encode_range b_index (b_offset - b_index) (c_offset - c_index));
					loop (c_offset + delimiter.slength) next;
				with Not_found ->
					DynArray.add acc (encode_range b_index (bl_this - b_index) (this.slength - c_index))
			in
			loop 0 0;
			encode_array_instance (EvalArray.create (DynArray.to_array acc))
		end
	)

	let substr = vifun2 (fun vthis pos len ->
		let this = this vthis in
		let cl_this = this.slength in
		let c_pos = decode_int pos in
		if c_pos >= cl_this then
			v_empty_string
		else begin
			let c_pos = if c_pos < 0 then begin
				let c_pos = this.slength + c_pos in
				if c_pos < 0 then 0 else c_pos
			end else c_pos in
			begin
				let c_len = match len with
					| VNull -> (cl_this - c_pos)
					| VInt32 i -> Int32.to_int i
					| _ -> unexpected_value len "int"
				in
				let c_len =
					if c_len < 0 then cl_this + c_len - c_pos
					else if c_len > cl_this - c_pos then cl_this - c_pos
					else c_len
				in
				vstring (substr this c_pos c_len);
			end
		end
	)

	let substring = vifun2 (fun vthis startIndex endIndex ->
		let this = this vthis in
		let c_first = decode_int startIndex in
		let cl_this = this.slength in
		let c_last = default_int endIndex cl_this in
		let c_first = if c_first < 0 then 0 else c_first in
		let c_last = if c_last < 0 then 0 else c_last in
		let c_first,c_last = if c_first > c_last then c_last,c_first else c_first,c_last in
		let c_last = if c_last > cl_this then cl_this else c_last in
		if c_first > cl_this || c_first = c_last then
			v_empty_string
		else begin
			begin
				let b_offset1 = get_offset this c_first in
				let c_len = c_last - c_first in
				let b_len =
					if c_last = cl_this then String.length this.sstring - b_offset1
					else (UTF8.move this.sstring b_offset1 c_len) - b_offset1
				in
				vstring (create_with_length (String.sub this.sstring b_offset1 b_len) c_len)
			end
		end
	)

	let toLowerCase = vifun0 (fun vthis ->
		let this = this vthis in
		vstring (case_map this false)
	)

	let toString = vifun0 (fun vthis -> vthis)

	let toUpperCase = vifun0 (fun vthis ->
		let this = this vthis in
		vstring (case_map this true)
	)

	let cca = charCodeAt
end

module StdStringBuf = struct
	let this vthis = match vthis with
		| VInstance {ikind = IBuffer sb} -> sb
		| v -> unexpected_value v "string"

	let add = vifun1 (fun vthis x ->
		let this = this vthis in
		let s = match x with
			| VString s -> s
			| _ -> create_ascii (value_string x)
		in
		VStringBuffer.add_string this s;
		vnull;
	)

	let addChar = vifun1 (fun vthis c ->
		let this = this vthis in
		let i = decode_int c in
		Buffer.add_string this.bbuffer (string_of_char_code i);
		this.blength <- this.blength + 1;
		vnull
	)

	let addSub = vifun3 (fun vthis s pos len ->
		let this = this vthis in
		let s = decode_vstring s in
		let c_pos = decode_int pos in
		let c_len = match len with
			| VNull -> s.slength - c_pos
			| VInt32 i -> Int32.to_int i
			| _ -> unexpected_value len "int"
		in
		if c_len > 0 then begin
			let b_offset1 = get_offset s c_pos in
			let b_offset2 = UTF8.move s.sstring b_offset1 c_len in
			VStringBuffer.add_substring this s b_offset1 (b_offset2 - b_offset1) c_len;
		end;
		vnull
	)

	let get_length = vifun0 (fun vthis ->
		let this = this vthis in
		vint this.blength
	)

	let toString = vifun0 (fun vthis ->
		let this = this vthis in
		let s = VStringBuffer.contents this in
		vstring s
	)
end

module StdStringTools = struct
	let url_encode s =
		let b = Buffer.create 0 in
		Common.url_encode s (Buffer.add_char b);
		Buffer.contents b

	let fastCodeAt = StdString.charCodeAt

	let replace = vfun3 (fun s sub by ->
		let by = decode_vstring by in
		let sub = decode_vstring sub in
		let s' = decode_vstring s in
		let bl_s = String.length s'.sstring in
		let buf = UTF8.Buf.create bl_s in
		let replace_count = ref 0 in
		let create () =
			vstring (create_with_length (UTF8.Buf.contents buf) (s'.slength + by.slength * !replace_count - sub.slength * !replace_count))
		in
		if sub.slength = 0 then begin
			if by.slength = 0 then
				s
			else begin
				UTF8.iter (fun uc ->
					UTF8.Buf.add_char buf uc;
					(* don't add for the final char *)
					if !replace_count <> s'.slength - 1 then begin
						UTF8.Buf.add_string buf by.sstring;
						incr replace_count;
					end
				) s'.sstring;
				create ();
			end
		end else begin
			let f = find_substring s' sub false in
			let rec loop c_index b_index =
				try
					let c_offset,b_offset,next = f c_index b_index in
					UTF8.Buf.add_string buf (String.sub s'.sstring b_index (b_offset - b_index));
					UTF8.Buf.add_string buf by.sstring;
					incr replace_count;
					loop (c_offset + sub.slength) next;
				with Not_found ->
					UTF8.Buf.add_string buf (String.sub s'.sstring b_index (bl_s - b_index));
			in
			loop 0 0;
			create()
		end
	)

	let urlEncode = vfun1 (fun s ->
		let s = decode_string s in
		encode_string (url_encode s)
	)

	let urlDecode = vfun1 (fun s ->
		let s = decode_string s in
		let b = VStringBuffer.create () in
		let add s =
			VStringBuffer.add_string b s
		in
		let len = String.length s in
		let decode c =
			match c with
			| '0'..'9' -> Some (int_of_char c - int_of_char '0')
			| 'a'..'f' -> Some (int_of_char c - int_of_char 'a' + 10)
			| 'A'..'F' -> Some (int_of_char c - int_of_char 'A' + 10)
			| _ -> None
		in
		let decode_hex i =
			let p1 = (try decode (String.get s i) with _ -> None) in
			let p2 = (try decode (String.get s (i + 1)) with _ -> None) in
			match p1, p2 with
			| Some c1, Some c2 ->
				Some (((c1 lsl 4) lor c2))
			| _ ->
				None
		in
		let expect_hex i =
			match String.unsafe_get s i with
			| '%' ->
				begin match decode_hex (i + 1) with
				| None -> exc_string "Malformed"
				| Some c -> c
				end
			| _ -> exc_string "Malformed"
		in
		let rec loop i =
			if i = len then () else
			let c = String.unsafe_get s i in
			match c with
			| '%' ->
				begin match decode_hex (i + 1) with
				| Some c ->
					if c < 0x80 then begin
						add (create_ascii (String.make 1 (char_of_int c)));
						loop (i + 3)
					end else if c < 0xE0 then begin
						let c2 = expect_hex (i + 3) in
						add (from_char_code (((c land 0x3F) lsl 6) lor (c2 land 0x7F)));
						loop (i + 6)
					end else if c < 0xF0 then begin
						let c2 = expect_hex (i + 3) in
						let c3 = expect_hex (i + 6) in
						add (from_char_code (((c land 0x1F) lsl 12) lor ((c2 land 0x7F) lsl 6) lor (c3 land 0x7F)));
						loop (i + 9)
					end else
						let c2 = expect_hex (i + 3) in
						let c3 = expect_hex (i + 6) in
						let c4 = expect_hex (i + 9) in
						let k = ((c land 0x0F) lsl 18) lor ((c2 land 0x7F) lsl 12) lor ((c3 land 0x7F) lsl 6) lor (c4 land 0x7F) in
						add (from_char_code k);
						loop (i + 12)
				| None ->
					loop (i + 1)
				end;
			| '+' ->
				add (create_ascii (String.make 1 ' '));
				loop (i + 1)
			| c ->
				add (create_ascii (String.make 1 c));
				loop (i + 1)
		in
		loop 0;
		vstring (VStringBuffer.contents b)
	)
end

module StdSys = struct
	open MacroApi
	open Common

	let args = vfun0 (fun () ->
		encode_array (List.map create_unknown ((get_ctx()).curapi.MacroApi.get_com()).args)
	)

	let _command = vfun1 (fun cmd ->
		let cmd = decode_string cmd in
		vint (((get_ctx()).curapi.get_com()).run_command cmd)
	)

	let cpuTime = vfun0 (fun () -> vfloat (Sys.time()))

	let environment = vfun0 (fun () ->
		let env = catch_unix_error Unix.environment() in
		let h = StringHashtbl.create () in
		Array.iter(fun s ->
			let k, v = ExtString.String.split s "=" in
			StringHashtbl.add h (create_ascii k) (create_unknown v)
		) env;
		encode_string_map_direct h
	)

	let exit = vfun1 (fun code ->
		raise (EvalTypes.Sys_exit(decode_int code));
	)

	let getChar = vfun1 (fun echo ->
		let echo = decode_bool echo in
		vint (Extc.getch echo)
	)

	let getCwd = vfun0 (fun () ->
		let dir = catch_unix_error Unix.getcwd() in
		let l = String.length dir in
		if l = 0 then
			encode_string "./"
		else match dir.[l - 1] with
		| '/' | '\\' ->
			create_unknown dir
		| _ ->
			create_unknown (dir ^ "/")
	)

	let getEnv = vfun1 (fun s ->
		let s = decode_string s in
		try create_unknown (catch_unix_error Unix.getenv s) with _ -> vnull
	)

	let print = vfun1 (fun v ->
		let ctx = get_ctx() in
		let com = ctx.curapi.get_com() in
		com.print (value_string v);
		vnull
	)

	let println = vfun1 (fun v ->
		let ctx = get_ctx() in
		let com = ctx.curapi.get_com() in
		com.print (value_string v ^ lineEnd);
		vnull
	)

	let programPath = vfun0 (fun () ->
		let ctx = get_ctx() in
		let com = ctx.curapi.get_com() in
		match com.main.main_class with
		| None -> vnull
		| Some p ->
			match ctx.curapi.get_type (s_type_path p) with
			| Some(Type.TInst (c, _)) -> create_unknown (Extc.get_full_path c.Type.cl_pos.Globals.pfile)
			| _ -> vnull
	)

	let putEnv = vfun2 (fun s -> function
		| v when v = vnull ->
			let _ = Luv.Env.unsetenv (decode_string s) in vnull
		| v ->
			let s = decode_string s in
			let v = decode_string v in
			catch_unix_error Unix.putenv s v;
			vnull
	)

	let setCwd = vfun1 (fun s ->
		catch_unix_error Unix.chdir (decode_string s);
		vnull
	)

	let setTimeLocale = vfun1 (fun _ -> vfalse)

	let sleep = vfun1 (fun f ->
		let time = Sys.time() in
		Thread.yield();
		let diff = Sys.time() -. time in
		Thread.delay ((num f) -. diff);
		vnull
	)

	let stderr = vfun0 (fun () ->
		encode_instance key_sys_io_FileOutput ~kind:(IOutChannel stderr)
	)

	let stdin = vfun0 (fun () ->
		encode_instance key_sys_io_FileInput ~kind:(IInChannel(stdin,ref false))
	)

	let stdout = vfun0 (fun () ->
		encode_instance key_sys_io_FileOutput ~kind:(IOutChannel stdout)
	)

	let systemName =
		let cached_sys_name = ref None in
		vfun0 (fun () ->
			let s = match Sys.os_type with
				| "Unix" ->
					(match !cached_sys_name with
					| Some n -> n
					| None ->
						let ic, pid = catch_unix_error Process_helper.open_process_args_in_pid "uname" [| "uname" |] in
						let uname = (match input_line ic with
							| "Darwin" -> "Mac"
							| n -> n
						) in
						Stdlib.ignore (Process_helper.close_process_in_pid (ic, pid));
						cached_sys_name := Some uname;
						uname)
				| "Win32" | "Cygwin" -> "Windows"
				| s -> s
			in
			encode_string s
		)

	let time = vfun0 (fun () -> vfloat (catch_unix_error Unix.gettimeofday()))
end

module StdThread = struct
	let this vthis = match vthis with
		| VInstance {ikind = IThread thread} -> thread
		| _ -> unexpected_value vthis "Thread"

	let delay = vfun1 (fun f ->
		Thread.delay (num f);
		vnull
	)

	let exit = vfun0 (fun () ->
		Thread.exit();
		vnull
	)

	let id = vifun0 (fun vthis ->
		vint (Thread.id (this vthis).tthread)
	)

	let get_events = vifun0 (fun vthis ->
		(this vthis).tevents
	)

	let set_events = vifun1 (fun vthis v ->
		(this vthis).tevents <- v;
		v
	)

	let join = vfun1 (fun thread ->
		Thread.join (this thread).tthread;
		vnull
	)

	(* Thread.kill has been marked deprecated (because unstable or even not working at all) for a while, and removed in ocaml 5 *)
	(* See also https://github.com/HaxeFoundation/haxe/issues/5800 *)
	(* let kill = vifun0 (fun vthis -> *)
	(* 	Thread.kill (this vthis).tthread; *)
	(* 	vnull *)
	(* ) *)

	let self = vfun0 (fun () ->
		let eval = get_eval (get_ctx()) in
		encode_instance key_eval_vm_Thread ~kind:(IThread eval.thread)
	)

	let readMessage = vfun1 (fun blocking ->
		let eval = get_eval (get_ctx()) in
		let blocking = decode_bool blocking in
		Option.get (Deque.pop eval.thread.tdeque blocking)
	)

	let sendMessage = vifun1 (fun vthis msg ->
		let this = this vthis in
		Deque.push this.tdeque msg;
		vnull
	)

	let yield = vfun0 (fun () ->
		Thread.yield();
		vnull
	)
end

module StdTls = struct
	let this vthis = match vthis with
		| VInstance {ikind = ITls i} -> i
		| _ -> unexpected_value vthis "Thread"

	let get_value = vifun0 (fun vthis ->
		let this = this vthis in
		try
			let id = Thread.id (Thread.self()) in
			let eval = IntMap.find id (get_ctx()).evals in
			IntMap.find this eval.thread.tstorage
		with Not_found ->
			vnull
	)

	let set_value = vifun1 (fun vthis v ->
		let this = this vthis in
		let eval = get_eval (get_ctx()) in
		eval.thread.tstorage <- IntMap.add this v eval.thread.tstorage;
		v
	)
end

module StdType = struct
	let create_enum v constr params =
		let vf = field v constr in
		match vf,params with
			| VEnumValue _,VNull -> vf
			| VEnumValue _,VArray va when va.alength = 0 -> vf
			| VFunction _,VArray va -> call_value vf (EvalArray.to_list va)
			| _ -> unexpected_value params "array"

	let allEnums = vfun1 (fun v ->
		match v with
		| VPrototype ({pkind = PEnum names} as proto) ->
			begin try
				let l = ExtList.List.filter_map (fun (s,_) ->
					try
						begin match proto_field_direct proto (hash s) with
							| VEnumValue _ as v -> Some v
							| _ -> None
						end
					with Not_found ->
						None
				) names in
				encode_array l
			with Not_found ->
				vnull
			end
		| _ ->
			vnull
	)

	let createEmptyInstance = vfun1 (fun v ->
		match v with
		| VPrototype {pkind = PClass _; ppath = path} ->
			begin try
				(Hashtbl.find (get_ctx()).builtins.empty_constructor_builtins path) ()
			with Not_found ->
				encode_instance path
			end
		| _ -> vnull
	)

	let createEnum = vfun3 (fun e constr params ->
		let constr = hash (decode_vstring constr).sstring in
		create_enum e constr params
	)

	let createEnumIndex = vfun3 (fun e index params ->
		let index = decode_int index in
		match e with
		| VPrototype {pkind = PEnum names} ->
			begin try
				create_enum e (hash (fst (List.nth names index))) params
			with Not_found ->
				vnull
			end
		| _ ->
			vnull
	)

	let createInstance = vfun2 (fun v vl ->
		match v with
		| VPrototype {pkind = PClass _; ppath = path} ->
			let ctx = get_ctx() in
			begin try
				let f = get_special_instance_constructor_raise ctx path in
				f (decode_array vl)
			with Not_found ->
				let vthis = encode_instance path in
				let fnew = get_instance_constructor ctx path null_pos in
				ignore(call_value_on vthis (Lazy.force fnew) (decode_array vl));
				vthis
			end
		| _ ->
			unexpected_value v "Class"
	)

	let enumConstructor = vfun1 (fun v -> match v with
		| VEnumValue ve ->
			begin try
				begin match (get_static_prototype_raise (get_ctx()) ve.epath).pkind with
					| PEnum names -> encode_string (fst (List.nth names ve.eindex))
					| _ -> raise Not_found
				end
			with Not_found ->
				vnull
			end
		| v -> unexpected_value v "enum value"
	)

	let enumEq = vfun2 (fun a b ->
		let rec weird_eq a b = match a,b with
			| VEnumValue a,VEnumValue b -> a == b || a.eindex = b.eindex && arrays_equal weird_eq a.eargs b.eargs && a.epath = b.epath
			| _ -> equals a b
		in
		vbool (weird_eq a b)
	)

	let enumIndex = vfun1 (fun v -> match v with
		| VEnumValue ev -> (try vint32 (Int32.of_int ev.eindex) with Not_found -> vnull)
		| v -> unexpected_value v "enum value"
	)

	let enumParameters = vfun1 (fun v -> match v with
		| VEnumValue ev ->
			let va = EvalArray.create ev.eargs in
			VArray va
		| v -> unexpected_value v "enum value"
	)

	let getClass = vfun1 (fun v ->
		match v with
		| VInstance ({iproto = {pkind = PInstance}} as vi) -> get_static_prototype_as_value (get_ctx()) vi.iproto.ppath null_pos
		| VString _ -> get_static_prototype_as_value (get_ctx()) key_String null_pos
		| VArray _ -> get_static_prototype_as_value (get_ctx()) key_Array null_pos
		| VVector _ -> get_static_prototype_as_value (get_ctx()) key_eval_Vector null_pos
		| _ -> vnull
	)

	let getClassFields = vfun1 (fun v ->
		match v with
		| VPrototype {pkind = PClass _;pnames = names} ->
			encode_array (IntMap.fold (fun name _ acc -> (encode_string (rev_hash name)) :: acc) names []);
		| _ ->
			vnull
	)

	let getClassName = vfun1 (fun v ->
		match v with
		| VPrototype {pkind = PClass _; ppath = path} -> encode_string (rev_hash path)
		| _ -> vnull
	)

	let getEnum = vfun1 (fun v ->
		match v with
		| VEnumValue ve -> get_static_prototype_as_value (get_ctx()) ve.epath null_pos
		| _ -> vnull
	)

	let getEnumConstructs = vfun1 (fun v ->
		match v with
		| VPrototype {pkind = PEnum names} ->
			begin try
				encode_array (List.map (fun (n,_) -> encode_string n) names)
			with Not_found ->
				vnull
			end
		| _ ->
			vnull
	)

	let getEnumName = vfun1 (fun v ->
		match v with
		| VPrototype {pkind = PEnum _; ppath = path} -> encode_string (rev_hash path)
		| _ -> vnull
	)

	let getInstanceFields = vfun1 (fun v ->
		match v with
		| VPrototype proto ->
			begin try
				let rec loop acc proto =
					let acc = match proto.pparent with
						| None -> acc
						| Some proto -> loop acc proto
					in
					let acc = IntMap.fold (fun name _ acc -> IntMap.add name 0 acc) proto.pinstance_names acc in
					IntMap.fold (fun name _ acc -> IntMap.add name 0 acc) proto.pnames acc
				in
				let proto = get_instance_prototype (get_ctx()) proto.ppath null_pos in
				encode_array (List.map (fun i -> encode_string (rev_hash i)) (ptmap_keys (loop IntMap.empty proto)))
			with Not_found ->
				vnull
			end
		| _ ->
			vnull
	)

	let getSuperClass = vfun1 (fun v ->
		match v with
		| VPrototype {pkind = PClass _; pparent = Some proto} -> proto.pvalue
		| _ -> vnull
	)

	let resolveClass = vfun1 (fun v ->
		let name = (decode_vstring v).sstring in
		try (get_static_prototype_raise (get_ctx()) (hash name)).pvalue with Not_found -> vnull
	)

	let resolveEnum = vfun1 (fun v ->
		let name = (decode_vstring v).sstring in
		try
			let proto = get_static_prototype_raise (get_ctx()) (hash name) in
			begin match proto.pkind with
				| PEnum _ -> proto.pvalue
				| _ -> vnull
			end
		with Not_found ->
			vnull
	)

	let typeof = vfun1 (fun v ->
		let ctx = (get_ctx()) in
		let rec loop v = match v with
			| VNull -> 0,[||]
			| VInt32 _ -> 1,[||]
			| VFloat _ -> 2,[||]
			| VTrue | VFalse -> 3,[||]
			| VInstance vi -> 6,[|get_static_prototype_as_value ctx vi.iproto.ppath null_pos|]
			| VString _ -> 6,[|get_static_prototype_as_value ctx key_String null_pos|]
			| VArray _ -> 6,[|get_static_prototype_as_value ctx key_Array null_pos|]
			| VVector _ -> 6,[|get_static_prototype_as_value ctx key_eval_Vector null_pos|]
			| VObject _ | VPrototype _ ->
				4,[||]
			| VFunction _
			| VFieldClosure _ ->
				5,[||]
			| VEnumValue ve ->
				7,[|get_static_prototype_as_value ctx ve.epath null_pos|]
			| VLazy f ->
				loop (!f())
			| VInt64 _ | VUInt64 _ | VNativeString _ | VHandle _ -> 8,[||]
		in
		let i,vl = loop v in
		encode_enum_value key_ValueType i vl None
	)
end

module StdUncompress = struct
	open Extc

	let this vthis = match vthis with
		| VInstance {ikind = IZip zip} -> zip
		| _ -> unexpected_value vthis "Uncompress"

	let close = vifun0 (fun vthis ->
		zlib_inflate_end (this vthis).z;
		vnull
	)

	let execute = vifun4 (fun vthis src srcPos dst dstPos ->
		StdCompress.exec zlib_inflate vthis src srcPos dst dstPos
	)

	let run = vfun2 (fun src bufsize ->
		let src = decode_bytes src in
		let bufsize = default_int bufsize (1 lsl 16) in
		let zip = zlib_inflate_init () in
		let buf = Buffer.create 0 in
		let tmp = Bytes.make bufsize (char_of_int 0) in
		let rec loop pos =
			let r = zlib_inflate zip (Bytes.unsafe_to_string src) pos (Bytes.length src - pos) tmp 0 bufsize Z_SYNC_FLUSH in
			Buffer.add_subbytes buf tmp 0 r.z_wrote;
			if not r.z_finish then loop (pos + r.z_read)
		in
		loop 0;
		encode_bytes (Bytes.unsafe_of_string (Buffer.contents buf))
	)

	let setFlushMode = StdCompress.setFlushMode
end

module StdUtf8 = struct
	let this vthis = match vthis with
		| VInstance {ikind = IUtf8 buf} -> buf
		| v -> unexpected_value v "string"

	let addChar = vifun1 (fun vthis c ->
		UTF8.Buf.add_char (this vthis) (UCharExt.uchar_of_int (decode_int c));
		vnull
	)

	let charCodeAt = StdString.charCodeAt

	let compare = vfun2 (fun a b ->
		let a = decode_string a in
		let b = decode_string b in
		vint (Stdlib.compare a b)
	)

	let decode = vfun1 (fun s ->
		let s = decode_string s in
		let buf = Bytes.create (UTF8.length s) in
		let i = ref 0 in
		UTF8.iter (fun uc ->
			Bytes.unsafe_set buf !i (UCharExt.char_of uc);
			incr i
		) s;
		let s = Bytes.unsafe_to_string buf in
		create_unknown s
	)

	let encode = vfun1 (fun s ->
		let s = decode_string s in
		create_unknown (UTF8.init (String.length s) (fun i -> UCharExt.of_char s.[i]))
	)

	let iter = vfun2 (fun s f ->
		let s = decode_string s in
		UTF8.iter (fun uc -> ignore(call_value f [vint (UCharExt.int_of_uchar uc)])) s;
		vnull
	)

	let length = vfun1 (fun s ->
		let s = decode_vstring s in
		vint (s.slength)
	)

	let sub = StdString.substr

	let toString = vifun0 (fun vthis ->
		let this = this vthis in
		vstring (create_ascii ((UTF8.Buf.contents this)))
	)

	let validate = vfun1 (fun s ->
		let s = decode_string s in
		try
			UTF8.validate s;
			vtrue
		with UTF8.Malformed_code ->
			vfalse
	)
end

module StdNativeString = struct
	let from_string = vfun1 (fun v ->
		match decode_optional decode_vstring v with
		| None -> vnull
		| Some s -> vnative_string s.sstring
	)

	let from_bytes = vfun1 (fun v ->
		let b = decode_bytes v in
		vnative_string (Bytes.to_string b)
	)

	let to_string = vfun1 (fun v ->
		let s = decode_native_string v in
		create_unknown s
	)

	let to_bytes = vfun1 (fun v ->
		let s = decode_native_string v in
		encode_bytes (Bytes.of_string s)
	)

	let concat = vfun2 (fun v1 v2 ->
		let s1 = decode_native_string v1
		and s2 = decode_native_string v2 in
		vnative_string (s1 ^ s2)
	)

	let char = vfun2 (fun v1 v2 ->
		let s = decode_native_string v1
		and index = decode_int v2 in
		try encode_string (String.make 1 s.[index])
		with Invalid_argument s -> throw_string s null_pos
	)

	let code = vfun2 (fun v1 v2 ->
		let s = decode_native_string v1
		and index = decode_int v2 in
		try vint (int_of_char s.[index])
		with Invalid_argument s -> throw_string s null_pos
	)

	let get_length = vfun1 (fun v ->
		let s = decode_native_string v in
		vint (String.length s)
	)

	let sub = vfun3 (fun v1 v2 v3 ->
		let s = decode_native_string v1
		and start = decode_int v2 in
		let max_length = String.length s - start in
		try
			if v3 = VNull then
				vnative_string (String.sub s start max_length)
			else
				let length =
					let l = decode_int v3 in
					if l > max_length then max_length else l
				in
				vnative_string (String.sub s start length)
		with Invalid_argument _ ->
			throw_string "Invalid arguments for eval.NativeString.sub" null_pos
	)
end

let init_fields builtins path static_fields instance_fields =
	let map (name,v) = (hash name,v) in
	let path = path_hash path in
	builtins.static_builtins <- IntMap.add path (List.map map static_fields) builtins.static_builtins;
	builtins.instance_builtins <- IntMap.add path (List.map map instance_fields) builtins.instance_builtins

let init_maps builtins =
	init_fields builtins (["haxe";"ds"],"IntMap") [] [
		"copy",StdIntMap.copy;
		"exists",StdIntMap.exists;
		"get",StdIntMap.get;
		"iterator",StdIntMap.iterator;
		"keys",StdIntMap.keys;
		"keyValueIterator",StdIntMap.keyValueIterator;
		"remove",StdIntMap.remove;
		"set",StdIntMap.set;
		"toString",StdIntMap.toString;
		"clear",StdIntMap.clear;
	];
	init_fields builtins (["haxe";"ds"],"ObjectMap") [] [
		"copy",StdObjectMap.copy;
		"exists",StdObjectMap.exists;
		"get",StdObjectMap.get;
		"iterator",StdObjectMap.iterator;
		"keys",StdObjectMap.keys;
		"keyValueIterator",StdObjectMap.keyValueIterator;
		"remove",StdObjectMap.remove;
		"set",StdObjectMap.set;
		"toString",StdObjectMap.toString;
		"clear",StdObjectMap.clear;
	];
	init_fields builtins (["haxe";"ds"],"StringMap") [] [
		"copy",StdStringMap.copy;
		"exists",StdStringMap.exists;
		"get",StdStringMap.get;
		"iterator",StdStringMap.iterator;
		"keys",StdStringMap.keys;
		"keyValueIterator",StdStringMap.keyValueIterator;
		"remove",StdStringMap.remove;
		"set",StdStringMap.set;
		"toString",StdStringMap.toString;
		"clear",StdStringMap.clear;
	]

let init_constructors builtins =
	let add = Hashtbl.add builtins.constructor_builtins in
	add key_Array (fun _ -> encode_array_instance (EvalArray.create [||]));
	add key_eval_Vector
		(fun vl ->
			match vl with
			| [size] ->
				encode_vector_instance (Array.make (decode_int size) vnull)
			| _ -> die "" __LOC__
		);
	add key_Date
		(fun vl ->
			begin match List.map decode_int vl with
			| [y;m;d;h;mi;s] ->
				let f = catch_unix_error (fun () ->
					let t = Unix.localtime 0. in
					Unix.mktime {t with tm_sec=s;tm_min=mi;tm_hour=h;tm_mday=d;tm_mon=m;tm_year=y - 1900}
				) () in
				encode_instance key_Date ~kind:(IDate (fst f))
			| _ -> die "" __LOC__
			end
		);
	add key_EReg
		(fun vl -> match vl with
			| [r;opt] -> encode_instance key_EReg ~kind:(StdEReg.create (decode_string r) (decode_string opt))
			| _ -> die "" __LOC__
		);
	add key_String
		(fun vl -> match vl with
			| [s] -> s
			| _ -> die "" __LOC__
		);
	add key_StringBuf (fun _ -> encode_instance key_StringBuf ~kind:(IBuffer (VStringBuffer.create())));
	add key_haxe_Utf8
		(fun vl -> match vl with
			| [size] -> encode_instance key_haxe_Utf8 ~kind:(IUtf8 (UTF8.Buf.create (default_int size 0)))
			| _ -> die "" __LOC__
		);
	add key_haxe_ds_StringMap (fun _ -> encode_string_map_direct (StringHashtbl.create ()));
	add key_haxe_ds_IntMap (fun _ -> encode_int_map_direct (IntHashtbl.create ()));
	add key_haxe_ds_ObjectMap (fun _ -> encode_object_map_direct (Obj.magic (ValueHashtbl.create 0)));
	add key_haxe_io_BytesBuffer (fun _ -> encode_instance key_haxe_io_BytesBuffer ~kind:(IOutput (Buffer.create 0)));
	add key_haxe_io_Bytes
		(fun vl -> match vl with
			| [length;b] ->
				let length = decode_int length in
				let b = decode_bytes b in
				let blit_length = if length > Bytes.length b then Bytes.length b else length in
				let b' = Bytes.create length in
				Bytes.blit b 0 b' 0 blit_length;
				encode_bytes b'
			| _ ->
				die "" __LOC__
		);
	add key_sys_io__Process_NativeProcess
		(fun vl -> match vl with
			| [cmd;args] ->
				let cmd = decode_string cmd in
				let args = match args with
					| VNull -> None
					| VArray va -> Some (Array.map decode_string (Array.sub va.avalues 0 va.alength))
					| _ -> unexpected_value args "array"
				in
				encode_instance key_sys_io__Process_NativeProcess ~kind:(IProcess (try Process.run cmd args with Failure msg -> exc_string msg))
			| _ -> die "" __LOC__
		);
	add key_eval_vm_NativeSocket
		(fun _ ->
			encode_instance key_eval_vm_NativeSocket ~kind:(ISocket ((catch_unix_error Unix.socket Unix.PF_INET Unix.SOCK_STREAM) 0))
		);
	add key_haxe_zip_Compress
		(fun vl -> match vl with
			| [level] ->
				let level = decode_int level in
				let z = Extc.zlib_deflate_init level in
				encode_instance key_haxe_zip_Compress ~kind:(IZip { z = z; z_flush = Extc.Z_NO_FLUSH })
			| _ -> die "" __LOC__
		);
	add key_haxe_zip_Uncompress
		(fun vl -> match vl with
			| [windowBits] ->
				let windowBits = default_int windowBits 15 in
				let z = Extc.zlib_inflate_init2 windowBits in
				encode_instance key_haxe_zip_Uncompress ~kind:(IZip { z = z; z_flush = Extc.Z_NO_FLUSH })
			| _ -> die "" __LOC__
		);
	add key_eval_vm_Thread
		(fun vl -> match vl with
			| [f] ->
				let ctx = get_ctx() in
				if ctx.is_macro then exc_string "Creating threads in macros is not supported";
				let thread = EvalThread.spawn ctx (fun () -> call_value f []) in
				encode_instance key_eval_vm_Thread ~kind:(IThread thread)
			| _ -> die "" __LOC__
		);
	add key_sys_net_Mutex
		(fun _ ->
			let mutex = {
				mmutex = Mutex.create();
				mowner = None;
			} in
			encode_instance key_sys_net_Mutex ~kind:(IMutex mutex)
		);
	add key_sys_net_Lock
		(fun _ ->
			let lock = {
				ldeque = Deque.create();
			} in
			encode_instance key_sys_net_Lock ~kind:(ILock lock)
		);
	let tls_counter = ref (-1) in
	add key_sys_net_Tls
		(fun _ ->
			incr tls_counter;
			encode_instance key_sys_net_Tls ~kind:(ITls !tls_counter)
		);
	add key_sys_net_Deque
		(fun _ ->
			encode_instance key_sys_net_Deque ~kind:(IDeque (Deque.create()))
		);
	EvalSsl.init_constructors add

let init_empty_constructors builtins =
	let h = builtins.empty_constructor_builtins in
	Hashtbl.add h key_Array (fun () -> encode_array_instance (EvalArray.create [||]));
	Hashtbl.add h key_eval_Vector (fun () -> encode_vector_instance (Array.make 0 vnull));
	Hashtbl.add h key_Date (fun () -> encode_instance key_Date ~kind:(IDate 0.));
	Hashtbl.add h key_EReg (fun () -> encode_instance key_EReg ~kind:(IRegex {r = Pcre2.regexp ""; r_rex_string = create_ascii "~//"; r_global = false; r_string = ""; r_groups = [||]}));
	Hashtbl.add h key_String (fun () -> v_empty_string);
	Hashtbl.add h key_haxe_ds_StringMap (fun () -> encode_instance key_haxe_ds_StringMap ~kind:(IStringMap (StringHashtbl.create ())));
	Hashtbl.add h key_haxe_ds_IntMap (fun () -> encode_instance key_haxe_ds_IntMap ~kind:(IIntMap (IntHashtbl.create ())));
	Hashtbl.add h key_haxe_ds_ObjectMap (fun () -> encode_instance key_haxe_ds_ObjectMap ~kind:(IObjectMap (Obj.magic (ValueHashtbl.create 0))));
	Hashtbl.add h key_haxe_io_BytesBuffer (fun () -> encode_instance key_haxe_io_BytesBuffer ~kind:(IOutput (Buffer.create 0)))

let init_standard_library builtins =
	init_constructors builtins;
	init_empty_constructors builtins;
	init_maps builtins;
	init_fields builtins ([],"Array") [] [
		"concat",StdArray.concat;
		"copy",StdArray.copy;
		"filter",StdArray.filter;
		"indexOf",StdArray.indexOf;
		"insert",StdArray.insert;
		"iterator",StdArray.iterator;
		"join",StdArray.join;
		"keyValueIterator",StdArray.keyValueIterator;
		"lastIndexOf",StdArray.lastIndexOf;
		"map",StdArray.map;
		"pop",StdArray.pop;
		"push",StdArray.push;
		"remove",StdArray.remove;
		"resize",StdArray.resize;
		"contains",StdArray.contains;
		"reverse",StdArray.reverse;
		"shift",StdArray.shift;
		"slice",StdArray.slice;
		"sort",StdArray.sort;
		"splice",StdArray.splice;
		"toString",StdArray.toString;
		"unshift",StdArray.unshift;
	];
	init_fields builtins (["eval"],"Vector") [
		"fromArrayCopy",StdEvalVector.fromArrayCopy;
	] [
		"blit",StdEvalVector.blit;
		"toArray",StdEvalVector.toArray;
		"copy",StdEvalVector.copy;
		"join",StdEvalVector.join;
		"map",StdEvalVector.map;
	];
	init_fields builtins (["haxe";"io"],"Bytes") [
		"alloc",StdBytes.alloc;
		"fastGet",StdBytes.fastGet;
		"ofData",StdBytes.ofData;
		"ofString",StdBytes.ofString;
		"ofHex",StdBytes.ofHex;
	] [
		"blit",StdBytes.blit;
		"compare",StdBytes.compare;
		"fill",StdBytes.fill;
		"get",StdBytes.get;
		"getData",StdBytes.getData;
		"getDouble",StdBytes.getDouble;
		"getFloat",StdBytes.getFloat;
		"getInt32",StdBytes.getInt32;
		"getInt64",StdBytes.getInt64;
		"getString",StdBytes.getString;
		"getUInt16",StdBytes.getUInt16;
		"set",StdBytes.set;
		"setDouble",StdBytes.setDouble;
		"setFloat",StdBytes.setFloat;
		"setInt32",StdBytes.setInt32;
		"setInt64",StdBytes.setInt64;
		"setUInt16",StdBytes.setUInt16;
		"sub",StdBytes.sub;
		"toHex",StdBytes.toHex;
		"toString",StdBytes.toString;
	];
	init_fields builtins (["haxe";"io"],"BytesBuffer") [] [
		"get_length",StdBytesBuffer.get_length;
		"addByte",StdBytesBuffer.addByte;
		"add",StdBytesBuffer.add;
		"addString",StdBytesBuffer.addString;
		"addInt32",StdBytesBuffer.addInt32;
		"addInt64",StdBytesBuffer.addInt64;
		"addFloat",StdBytesBuffer.addFloat;
		"addDouble",StdBytesBuffer.addDouble;
		"addBytes",StdBytesBuffer.addBytes;
		"getBytes",StdBytesBuffer.getBytes;
	];
	init_fields builtins (["haxe"],"NativeStackTrace") [
		"_callStack",EvalStackTrace.getCallStack;
		"exceptionStack",EvalStackTrace.getExceptionStack;
	] [];
	init_fields builtins (["haxe";"zip"],"Compress") [
		"run",StdCompress.run;
	] [
		"close",StdCompress.close;
		"execute",StdCompress.execute;
		"setFlushMode",StdCompress.setFlushMode;
	];
	init_fields builtins (["eval";"vm"],"Context") [
		"addBreakpoint",StdContext.addBreakpoint;
		"breakHere",StdContext.breakHere;
		"callMacroApi",StdContext.callMacroApi;
		"loadPlugin",StdContext.loadPlugin;
	] [];
	init_fields builtins (["haxe";"crypto"],"Crc32") [
		"make",StdCrc32.make;
	] [];
	init_fields builtins ([],"Date") [
		"fromString",StdDate.fromString;
		"fromTime",StdDate.fromTime;
		"now",StdDate.now;
	] [
		"getDate",StdDate.getDate;
		"getDay",StdDate.getDay;
		"getFullYear",StdDate.getFullYear;
		"getHours",StdDate.getHours;
		"getMinutes",StdDate.getMinutes;
		"getMonth",StdDate.getMonth;
		"getSeconds",StdDate.getSeconds;
		"getUTCDate",StdDate.getUTCDate;
		"getUTCDay",StdDate.getUTCDay;
		"getUTCFullYear",StdDate.getUTCFullYear;
		"getUTCHours",StdDate.getUTCHours;
		"getUTCMinutes",StdDate.getUTCMinutes;
		"getUTCMonth",StdDate.getUTCMonth;
		"getUTCSeconds",StdDate.getUTCSeconds;
		"getTime",StdDate.getTime;
		"getTimezoneOffset",StdDate.getTimezoneOffset;
		"toString",StdDate.toString;
	];
	init_fields builtins (["sys";"thread"],"Deque") [] [
		"add",StdDeque.add;
		"push",StdDeque.push;
		"pop",StdDeque.pop;
	];
	init_fields builtins ([],"EReg") [
		"escape",StdEReg.escape;
	] [
		"map",StdEReg.map;
		"match",StdEReg.match';
		"matched",StdEReg.matched;
		"matchedLeft",StdEReg.matchedLeft;
		"matchedPos",StdEReg.matchedPos;
		"matchedRight",StdEReg.matchedRight;
		"matchSub",StdEReg.matchSub;
		"replace",StdEReg.replace;
		"split",StdEReg.split;
	];
	init_fields builtins (["sys";"io"],"File") [
		"append",StdFile.append;
		"getBytes",StdFile.getBytes;
		"getContent",StdFile.getContent;
		"read",StdFile.read;
		"saveBytes",StdFile.saveBytes;
		"saveContent",StdFile.saveContent;
		"update",StdFile.update;
		"write",StdFile.write;
	] [];
	init_fields builtins (["sys";"io"],"FileInput") [] [
		"close",StdFileInput.close;
		"eof",StdFileInput.eof;
		"seek",StdFileInput.seek;
		"tell",StdFileInput.tell;
		"readByte",StdFileInput.readByte;
		"readBytes",StdFileInput.readBytes;
	];
	init_fields builtins (["sys";"io"],"FileOutput") [] [
		"close",StdFileOutput.close;
		"flush",StdFileOutput.flush;
		"seek",StdFileOutput.seek;
		"tell",StdFileOutput.tell;
		"writeByte",StdFileOutput.writeByte;
		"writeBytes",StdFileOutput.writeBytes;
	];
	init_fields builtins (["haxe";"io"],"FPHelper") [
		"doubleToI64",StdFPHelper.doubleToI64;
		"floatToI32",StdFPHelper.floatToI32;
		"i32ToFloat",StdFPHelper.i32ToFloat;
		"i64ToDouble",StdFPHelper.i64ToDouble;
	] [];
	init_fields builtins (["sys"],"FileSystem") [
		"createDirectory",StdFileSystem.createDirectory;
		"deleteFile",StdFileSystem.deleteFile;
		"deleteDirectory",StdFileSystem.deleteDirectory;
		"exists",StdFileSystem.exists;
		"fullPath",StdFileSystem.fullPath;
		"isDirectory",StdFileSystem.isDirectory;
		"rename",StdFileSystem.rename;
		"readDirectory",StdFileSystem.readDirectory;
		"stat",StdFileSystem.stat;
	] [];
	init_fields builtins (["eval";"vm"],"Gc") [
		"allocated_bytes",StdGc.allocated_bytes;
		"compact",StdGc.compact;
		"counters",StdGc.counters;
		"finalise",StdGc.finalise;
		"finalise_release",StdGc.finalise_release;
		"full_major",StdGc.full_major;
		"get",StdGc.get;
		"major",StdGc.major;
		"major_slice",StdGc.major_slice;
		"minor",StdGc.minor;
		"print_stat",StdGc.print_stat;
		"quick_stat",StdGc.quick_stat;
		"set",StdGc.set;
		"stat",StdGc.stat;
	] [];
	init_fields builtins (["sys";"net"],"Host") [
		"localhost",StdHost.localhost;
		"hostReverse",StdHost.hostReverse;
		"hostToString",StdHost.hostToString;
		"resolve",StdHost.resolve;
	] [];
	init_fields builtins (["sys";"thread"],"Lock") [] [
		"release",StdLock.release;
		"wait",StdLock.wait;
	];
	init_fields builtins (["haxe"],"Log") [
		"trace",StdLog.trace;
	] [];
	init_fields builtins ([],"Math") [
		"NaN",StdMath.nan;
		"NEGATIVE_INFINITY",StdMath.negative_infinity;
		"PI",StdMath.pi;
		"POSITIVE_INFINITY",StdMath.positive_infinity;
		"abs",StdMath.abs;
		"acos",StdMath.acos;
		"asin",StdMath.asin;
		"atan",StdMath.atan;
		"atan2",StdMath.atan2;
		"ceil",StdMath.ceil;
		"cos",StdMath.cos;
		"exp",StdMath.exp;
		"fceil",StdMath.fceil;
		"ffloor",StdMath.ffloor;
		"floor",StdMath.floor;
		"fround",StdMath.fround;
		"isFinite",StdMath.isFinite;
		"isNaN",StdMath.isNaN;
		"log",StdMath.log;
		"max",StdMath.max;
		"min",StdMath.min;
		"pow",StdMath.pow;
		"random",StdMath.random;
		"round",StdMath.round;
		"sin",StdMath.sin;
		"sqrt",StdMath.sqrt;
		"tan",StdMath.tan;
	] [];
	init_fields builtins (["haxe";"crypto"],"Md5") [
		"encode",StdMd5.encode;
		"make",StdMd5.make;
	] [];
	init_fields builtins (["sys";"thread"],"Mutex") [] [
		"acquire",StdMutex.acquire;
		"tryAcquire",StdMutex.tryAcquire;
		"release",StdMutex.release;
	];
	init_fields builtins (["sys";"io";"_Process"],"NativeProcess") [ ] [
		"close",StdNativeProcess.close;
		"exitCode",StdNativeProcess.exitCode;
		"getPid",StdNativeProcess.getPid;
		"kill",StdNativeProcess.kill;
		"readStderr",StdNativeProcess.readStderr;
		"readStdout",StdNativeProcess.readStdout;
		"closeStdin",StdNativeProcess.closeStdin;
		"writeStdin",StdNativeProcess.writeStdin;
	];
	init_fields builtins ([],"Reflect") [
		"callMethod",StdReflect.callMethod;
		"compare",StdReflect.compare;
		"compareMethods",StdReflect.compareMethods;
		"copy",StdReflect.copy;
		"deleteField",StdReflect.deleteField;
		"field",StdReflect.field';
		"fields",StdReflect.fields;
		"getProperty",StdReflect.getProperty;
		"hasField",StdReflect.hasField;
		"isEnumValue",StdReflect.isEnumValue;
		"isFunction",StdReflect.isFunction;
		"isObject",StdReflect.isObject;
		"makeVarArgs",StdReflect.makeVarArgs;
		"setField",StdReflect.setField;
		"setProperty",StdReflect.setProperty;
	] [];
	init_fields builtins (["haxe"],"Resource") [
		"listNames",StdResource.listNames;
		"getString",StdResource.getString;
		"getBytes",StdResource.getBytes;
	] [];
	init_fields builtins (["haxe";"crypto"],"Sha1") [
		"encode",StdSha1.encode;
		"make",StdSha1.make;
	] [];
	init_fields builtins (["eval";"vm"],"NativeSocket") [
		"select",StdSocket.select;
	] [
		"accept",StdSocket.accept;
		"bind",StdSocket.bind;
		"close",StdSocket.close;
		"connect",StdSocket.connect;
		"host",StdSocket.host;
		"listen",StdSocket.listen;
		"peer",StdSocket.peer;
		"receive",StdSocket.receive;
		"receiveChar",StdSocket.receiveChar;
		"send",StdSocket.send;
		"sendChar",StdSocket.sendChar;
		"setFastSend",StdSocket.setFastSend;
		"setBroadcast", StdSocket.setBroadcast;
		"setTimeout",StdSocket.setTimeout;
		"shutdown",StdSocket.shutdown;
	];
	init_fields builtins ([],"Std") [
		"downcast",StdStd.downcast;
		"instance",StdStd.instance;
		"int",StdStd.int;
		"is",StdStd.is';
		"isOfType",StdStd.isOfType;
		"parseFloat",StdStd.parseFloat;
		"parseInt",StdStd.parseInt;
		"string",StdStd.string;
		"random",StdStd.random;
	] [];
	init_fields builtins ([],"String") [
		"fromCharCode",StdString.fromCharCode;
	] [
		"charAt",StdString.charAt;
		"charCodeAt",StdString.charCodeAt;
		"indexOf",StdString.indexOf;
		"lastIndexOf",StdString.lastIndexOf;
		"split",StdString.split;
		"substr",StdString.substr;
		"substring",StdString.substring;
		"toLowerCase",StdString.toLowerCase;
		"toString",StdString.toString;
		"toUpperCase",StdString.toUpperCase;
		"cca",StdString.cca;
	];
	init_fields builtins ([],"StringBuf") [] [
		"add",StdStringBuf.add;
		"addChar",StdStringBuf.addChar;
		"addSub",StdStringBuf.addSub;
		"get_length",StdStringBuf.get_length;
		"toString",StdStringBuf.toString;
	];
	init_fields builtins ([],"StringTools") [
		"fastCodeAt",StdStringTools.fastCodeAt;
		"replace",StdStringTools.replace;
		"urlEncode",StdStringTools.urlEncode;
		"urlDecode",StdStringTools.urlDecode;
	] [];
	init_fields builtins ([],"Sys") [
		"args",StdSys.args;
		"_command",StdSys._command;
		"cpuTime",StdSys.cpuTime;
		"environment",StdSys.environment;
		"exit",StdSys.exit;
		"getChar",StdSys.getChar;
		"getCwd",StdSys.getCwd;
		"getEnv",StdSys.getEnv;
		"print",StdSys.print;
		"println",StdSys.println;
		"programPath",StdSys.programPath;
		"putEnv",StdSys.putEnv;
		"setCwd",StdSys.setCwd;
		"setTimeLocale",StdSys.setTimeLocale;
		"sleep",StdSys.sleep;
		"stderr",StdSys.stderr;
		"stdin",StdSys.stdin;
		"stdout",StdSys.stdout;
		"systemName",StdSys.systemName;
		"time",StdSys.time;
	] [];
	init_fields builtins (["eval";"vm"],"NativeThread") [
		"delay",StdThread.delay;
		"exit",StdThread.exit;
		"join",StdThread.join;
		"readMessage",StdThread.readMessage;
		"self",StdThread.self;
		"yield",StdThread.yield;
	] [
		"id",StdThread.id;
		"get_events",StdThread.get_events;
		"set_events",StdThread.set_events;
		(* "kill",StdThread.kill; *)
		"sendMessage",StdThread.sendMessage;
	];
	init_fields builtins (["sys";"thread"],"Tls") [] [
		"get_value",StdTls.get_value;
		"set_value",StdTls.set_value;
	];
	init_fields builtins ([],"Type") [
		"allEnums",StdType.allEnums;
		"createEmptyInstance",StdType.createEmptyInstance;
		"createEnum",StdType.createEnum;
		"createEnumIndex",StdType.createEnumIndex;
		"createInstance",StdType.createInstance;
		"enumConstructor",StdType.enumConstructor;
		"enumEq",StdType.enumEq;
		"enumIndex",StdType.enumIndex;
		"enumParameters",StdType.enumParameters;
		"getClass",StdType.getClass;
		"getClassFields",StdType.getClassFields;
		"getClassName",StdType.getClassName;
		"getEnum",StdType.getEnum;
		"getEnumConstructs",StdType.getEnumConstructs;
		"getEnumName",StdType.getEnumName;
		"getInstanceFields",StdType.getInstanceFields;
		"getSuperClass",StdType.getSuperClass;
		"resolveClass",StdType.resolveClass;
		"resolveEnum",StdType.resolveEnum;
		"typeof",StdType.typeof;
	] [];
	init_fields builtins (["haxe";"zip"],"Uncompress") [
		"run",StdUncompress.run;
	] [
		"close",StdUncompress.close;
		"execute",StdUncompress.execute;
		"setFlushMode",StdUncompress.setFlushMode;
	];
	init_fields builtins (["haxe"],"Utf8") [
		"charCodeAt",StdUtf8.charCodeAt;
		"compare",StdUtf8.compare;
		"decode",StdUtf8.decode;
		"encode",StdUtf8.encode;
		"iter",StdUtf8.iter;
		"length",StdUtf8.length;
		"sub",StdUtf8.sub;
		"validate",StdUtf8.validate;
	] [
		"addChar",StdUtf8.addChar;
		"toString",StdUtf8.toString;
	];
	init_fields builtins (["eval";"_NativeString"],"NativeString_Impl_") [
		"fromBytes",StdNativeString.from_bytes;
		"fromString",StdNativeString.from_string;
		"toBytes",StdNativeString.to_bytes;
		"toString",StdNativeString.to_string;
		"concat",StdNativeString.concat;
		"char",StdNativeString.char;
		"code",StdNativeString.code;
		"get_length",StdNativeString.get_length;
		"sub",StdNativeString.sub;
	] [];
	init_fields builtins (["eval";"integers";"_UInt64"],"UInt64_Impl_") EvalIntegers.uint64_fields [];
	init_fields builtins (["eval";"integers";"_Int64"],"Int64_Impl_") EvalIntegers.int64_fields [];
	init_fields builtins (["eval";"luv";"_UVError"],"UVError_Impl_") EvalLuv.uv_error_fields [];
	init_fields builtins (["eval";"luv";"_Loop"],"Loop_Impl_") EvalLuv.loop_fields [];
	init_fields builtins (["eval";"luv";"_Loop"],"LoopOption_Impl_") ["sigprof",vint Luv.Loop.Option.sigprof] [];
	init_fields builtins (["eval";"luv";"_Handle"],"Handle_Impl_") EvalLuv.handle_fields [];
	init_fields builtins (["eval";"luv";"_Idle"], "Idle_Impl_") EvalLuv.idle_fields [];
	init_fields builtins (["eval";"luv";"_Async"], "Async_Impl_") EvalLuv.async_fields [];
	init_fields builtins (["eval";"luv";"_Timer"], "Timer_Impl_") EvalLuv.timer_fields [];
	init_fields builtins (["eval";"luv";"_Buffer"], "Buffer_Impl_") EvalLuv.buffer_fields [];
	init_fields builtins (["eval";"luv";"_SockAddr"], "SockAddr_Impl_") EvalLuv.sockaddr_fields [];
	init_fields builtins (["eval";"luv";"_Tcp"], "Tcp_Impl_") EvalLuv.tcp_fields [];
	init_fields builtins (["eval";"luv";"_Udp"], "Udp_Impl_") EvalLuv.udp_fields [];
	init_fields builtins (["eval";"luv";"_ConnectedUdp"], "ConnectedUdp_Impl_") EvalLuv.connected_udp_fields [];
	init_fields builtins (["eval";"luv";"_Pipe"], "Pipe_Impl_") EvalLuv.pipe_fields [];
	init_fields builtins (["eval";"luv";"_Tty"], "Tty_Impl_") EvalLuv.tty_fields [];
	init_fields builtins (["eval";"luv";"_Stream"], "Stream_Impl_") EvalLuv.stream_fields [];
	init_fields builtins (["eval";"luv";"_Signal"], "Signal_Impl_") EvalLuv.signal_fields [];
	init_fields builtins (["eval";"luv";"_Signal"], "SigNum_Impl_") EvalLuv.signum_fields [];
	init_fields builtins (["eval";"luv";"_Process"], "Process_Impl_") EvalLuv.process_fields [];
	init_fields builtins (["eval";"luv";"_Request"], "Request_Impl_") EvalLuv.request_fields [];
	init_fields builtins (["eval";"luv"], "Dns") EvalLuv.dns_fields [];
	init_fields builtins (["eval";"luv";"_File"], "File_Impl_") EvalLuv.file_fields [];
	init_fields builtins (["eval";"luv";"_Dir"], "Dir_Impl_") EvalLuv.dir_fields [];
	init_fields builtins (["eval";"luv"], "FileSync") EvalLuv.file_sync_fields [];
	init_fields builtins (["eval";"luv"], "DirSync") EvalLuv.dir_sync_fields [];
	init_fields builtins (["eval";"luv";"_FsEvent"], "FsEvent_Impl_") EvalLuv.fs_event_fields [];
	init_fields builtins (["eval";"luv"], "ThreadPool") EvalLuv.thread_pool_fields [];
	init_fields builtins (["eval";"luv";"_Thread"], "Thread_Impl_") EvalLuv.thread_fields [];
	init_fields builtins (["eval";"luv";"_Once"], "Once_Impl_") EvalLuv.once_fields [];
	init_fields builtins (["eval";"luv";"_Mutex"], "Mutex_Impl_") EvalLuv.mutex_fields [];
	init_fields builtins (["eval";"luv";"_RwLock"], "RwLock_Impl_") EvalLuv.rwlock_fields [];
	init_fields builtins (["eval";"luv";"_Semaphore"], "Semaphore_Impl_") EvalLuv.semaphore_fields [];
	init_fields builtins (["eval";"luv";"_Condition"], "Condition_Impl_") EvalLuv.condition_fields [];
	init_fields builtins (["eval";"luv";"_Barrier"], "Barrier_Impl_") EvalLuv.barrier_fields [];
	init_fields builtins (["eval";"luv"], "Env") EvalLuv.env_fields [];
	init_fields builtins (["eval";"luv"], "Time") EvalLuv.time_fields [];
	init_fields builtins (["eval";"luv"], "Path") EvalLuv.path_fields [];
	init_fields builtins (["eval";"luv"], "Random") EvalLuv.random_fields [];
	init_fields builtins (["eval";"luv"], "RandomSync") EvalLuv.random_sync_fields [];
	init_fields builtins (["eval";"luv"], "Network") EvalLuv.network_fields [];
	init_fields builtins (["eval";"luv";"_FsPoll"], "FsPoll_Impl_") EvalLuv.fs_poll_fields [];
	init_fields builtins (["eval";"luv"], "Resource") EvalLuv.resource_fields [];
	init_fields builtins (["eval";"luv"], "SystemInfo") EvalLuv.system_info_fields [];
	init_fields builtins (["eval";"luv"], "Pid") EvalLuv.pid_fields [];
	init_fields builtins (["eval";"luv"], "Passwd") EvalLuv.passwd_fields [];
	init_fields builtins (["eval";"luv"], "Metrics") EvalLuv.metrics_fields [];
	init_fields builtins (["eval";"luv";"_Prepare"], "Prepare_Impl_") EvalLuv.prepare_fields [];
	init_fields builtins (["eval";"luv";"_Check"], "Check_Impl_") EvalLuv.check_fields [];
	init_fields builtins (["eval";"luv"], "Version") EvalLuv.version_fields [];
	EvalSsl.init_fields init_fields builtins
