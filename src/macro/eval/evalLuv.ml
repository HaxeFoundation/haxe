open Luv
open Globals
open EvalContext
open EvalExceptions
open EvalValue
open EvalEncode
open EvalDecode
open EvalHash
open EvalMisc

let key_eval_luv_Result = hash "eval.luv.Result"

let encode_uv_error (e:Error.t) =
	vint (match e with
	| `E2BIG -> 0
	| `EACCES -> 1
	| `EADDRINUSE -> 2
	| `EADDRNOTAVAIL -> 3
	| `EAFNOSUPPORT -> 4
	| `EAGAIN -> 5
	| `EAI_ADDRFAMILY -> 6
	| `EAI_AGAIN -> 7
	| `EAI_BADFLAGS -> 8
	| `EAI_BADHINTS -> 9
	| `EAI_CANCELED -> 10
	| `EAI_FAIL -> 11
	| `EAI_FAMILY -> 12
	| `EAI_MEMORY -> 13
	| `EAI_NODATA -> 14
	| `EAI_NONAME -> 15
	| `EAI_OVERFLOW -> 16
	| `EAI_PROTOCOL -> 17
	| `EAI_SERVICE -> 18
	| `EAI_SOCKTYPE -> 19
	| `EALREADY -> 20
	| `EBADF -> 21
	| `EBUSY -> 22
	| `ECANCELED -> 23
	(* | `ECHARSET -> 24; not defined in Luv *)
	| `ECONNABORTED -> 25
	| `ECONNREFUSED -> 26
	| `ECONNRESET -> 27
	| `EDESTADDRREQ -> 28
	| `EEXIST -> 29
	| `EFAULT -> 30
	| `EFBIG -> 31
	| `EHOSTUNREACH -> 32
	| `EINTR -> 33
	| `EINVAL -> 34
	| `EIO -> 35
	| `EISCONN -> 36
	| `EISDIR -> 37
	| `ELOOP -> 38
	| `EMFILE -> 39
	| `EMSGSIZE -> 40
	| `ENAMETOOLONG -> 41
	| `ENETDOWN -> 42
	| `ENETUNREACH -> 43
	| `ENFILE -> 44
	| `ENOBUFS -> 45
	| `ENODEV -> 46
	| `ENOENT -> 47
	| `ENOMEM -> 48
	| `ENONET -> 49
	| `ENOPROTOOPT -> 50
	| `ENOSPC -> 51
	| `ENOSYS -> 52
	| `ENOTCONN -> 53
	| `ENOTDIR -> 54
	| `ENOTEMPTY -> 55
	| `ENOTSOCK -> 56
	| `ENOTSUP -> 57
	| `EPERM -> 58
	| `EPIPE -> 59
	| `EPROTO -> 60
	| `EPROTONOSUPPORT -> 61
	| `EPROTOTYPE -> 62
	| `ERANGE -> 63
	| `EROFS -> 64
	| `ESHUTDOWN -> 65
	| `ESPIPE -> 66
	| `ESRCH -> 67
	| `ETIMEDOUT -> 68
	| `ETXTBSY -> 69
	| `EXDEV -> 70
	| `UNKNOWN -> 71
	| `EOF -> 72
	| `ENXIO -> 73
	| `EMLINK -> 74
	| `ENOTTY -> 75
	| `EFTYPE -> 76
	| `EILSEQ -> 77
	)

let luv_exception e =
	let vi = encode_instance key_eval_luv_LuvException in
	match vi with
	| VInstance i ->
		let msg = EvalString.create_unknown (Error.strerror e)
		and error = encode_uv_error e in
		set_instance_field i key_exception_message msg;
		set_instance_field i key_native_exception error;
		set_instance_field i key_error error;
		let ctx = get_ctx() in
		let eval = get_eval ctx in
		(match eval.env with
		| Some _ ->
			let stack = EvalStackTrace.make_stack_value (call_stack eval) in
			set_instance_field i key_native_stack stack;
		| None -> ());
		vi
	| _ ->
		die "" __LOC__

let encode_result f result =
	let index, args =
		match result with
		| Ok r -> 0, [|f r|]
		| Error e -> 1, [|encode_uv_error e|]
	in
	encode_enum_value key_eval_luv_Result index args None

let encode_unit_result result =
	let index, args =
		match result with
		| Ok r -> 0, [|vnull|]
		| Error e -> 1, [|encode_uv_error e|]
	in
	encode_enum_value key_eval_luv_Result index args None

let resolve_result = function
	| Ok v -> v
	| Error e -> throw (luv_exception e) null_pos

let decode_loop v =
	match decode_handle v with
	| HLoop t -> t
	| _ -> unexpected_value v "eval.luv.Loop"

let decode_loop_opt v =
	if v = vnull then Loop.default()
	else decode_loop v

let decode_luv_handle v : 'kind Luv.Handle.t =
	match decode_handle v with
	| HIdle t -> t
	| _ -> unexpected_value v "eval.luv.Handle"

let decode_idle v =
	match decode_handle v with
	| HIdle t -> t
	| _ -> unexpected_value v "eval.luv.Idle.IdleHandle"

let loop_run = vfun2 (fun v1 v2 ->
	let loop = decode_loop_opt v1
	and mode =
		match decode_int v2 with
		| 0 -> `DEFAULT
		| 1 -> `ONCE
		| 2 -> `NOWAIT
		| _ -> unexpected_value v2 "valid loop run mode"
	in
	vbool (Loop.run ~loop ~mode ())
)

let loop_stop = vfun1 (fun v ->
	let loop = decode_loop v in
	Loop.stop loop;
	vnull
)

let loop_init = vfun0 (fun () ->
	encode_result (fun l -> encode_handle (HLoop l)) (Loop.init())
)

let loop_close = vfun1 (fun v ->
	let loop = decode_loop v in
	encode_unit_result (Loop.close loop)
)

let loop_alive = vfun1 (fun v ->
	let loop = decode_loop v in
	vbool (Loop.alive loop)
)

let loop_default = vfun0 (fun () ->
	encode_handle (HLoop (Loop.default()))
)

let handle_close = vfun2 (fun v1 v2 ->
	let handle = decode_luv_handle v1
	and cb = prepare_callback v2 0 in
	Handle.close handle (fun() -> ignore(cb []));
	vnull
)

let handle_is_active = vfun1 (fun v ->
	let handle = decode_luv_handle v in
	vbool (Handle.is_active handle)
)

let handle_is_closing = vfun1 (fun v ->
	let handle = decode_luv_handle v in
	vbool (Handle.is_closing handle)
)

let handle_ref = vfun1 (fun v ->
	let handle = decode_luv_handle v in
	Handle.ref handle;
	vnull
)

let handle_unref = vfun1 (fun v ->
	let handle = decode_luv_handle v in
	Handle.unref handle;
	vnull
)

let handle_has_ref = vfun1 (fun v ->
	let handle = decode_luv_handle v in
	vbool (Handle.has_ref handle)
)

let idle_init = vfun1 (fun v ->
	let loop = decode_loop_opt v in
	encode_result (fun i -> encode_handle (HIdle i)) (Idle.init ~loop ())
)

let idle_start = vfun2 (fun v1 v2 ->
	let idle = decode_idle v1
	and cb = prepare_callback v2 0 in
	encode_unit_result (Idle.start idle (fun() -> ignore(cb [])));
)

let idle_stop = vfun1 (fun v ->
	let idle = decode_idle v in
	resolve_result (Idle.stop idle);
	vnull;
)