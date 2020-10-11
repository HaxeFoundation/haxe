module HaxeError = Error

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
let key_eval_luv_LuvException = hash "eval.luv.LuvException"
let key_eval_luv_ReceiveHandle = hash "eval.luv.ReceiveHandle"

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

let decode_uv_error v : Error.t =
	match decode_int v with
	| 0 -> `E2BIG
	| 1 -> `EACCES
	| 2 -> `EADDRINUSE
	| 3 -> `EADDRNOTAVAIL
	| 4 -> `EAFNOSUPPORT
	| 5 -> `EAGAIN
	| 6 -> `EAI_ADDRFAMILY
	| 7 -> `EAI_AGAIN
	| 8 -> `EAI_BADFLAGS
	| 9 -> `EAI_BADHINTS
	| 10 -> `EAI_CANCELED
	| 11 -> `EAI_FAIL
	| 12 -> `EAI_FAMILY
	| 13 -> `EAI_MEMORY
	| 14 -> `EAI_NODATA
	| 15 -> `EAI_NONAME
	| 16 -> `EAI_OVERFLOW
	| 17 -> `EAI_PROTOCOL
	| 18 -> `EAI_SERVICE
	| 19 -> `EAI_SOCKTYPE
	| 20 -> `EALREADY
	| 21 -> `EBADF
	| 22 -> `EBUSY
	| 23 -> `ECANCELED
	(* | 24 -> `ECHARSET not defined in Luv *)
	| 25 -> `ECONNABORTED
	| 26 -> `ECONNREFUSED
	| 27 -> `ECONNRESET
	| 28 -> `EDESTADDRREQ
	| 29 -> `EEXIST
	| 30 -> `EFAULT
	| 31 -> `EFBIG
	| 32 -> `EHOSTUNREACH
	| 33 -> `EINTR
	| 34 -> `EINVAL
	| 35 -> `EIO
	| 36 -> `EISCONN
	| 37 -> `EISDIR
	| 38 -> `ELOOP
	| 39 -> `EMFILE
	| 40 -> `EMSGSIZE
	| 41 -> `ENAMETOOLONG
	| 42 -> `ENETDOWN
	| 43 -> `ENETUNREACH
	| 44 -> `ENFILE
	| 45 -> `ENOBUFS
	| 46 -> `ENODEV
	| 47 -> `ENOENT
	| 48 -> `ENOMEM
	| 49 -> `ENONET
	| 50 -> `ENOPROTOOPT
	| 51 -> `ENOSPC
	| 52 -> `ENOSYS
	| 53 -> `ENOTCONN
	| 54 -> `ENOTDIR
	| 55 -> `ENOTEMPTY
	| 56 -> `ENOTSOCK
	| 57 -> `ENOTSUP
	| 58 -> `EPERM
	| 59 -> `EPIPE
	| 60 -> `EPROTO
	| 61 -> `EPROTONOSUPPORT
	| 62 -> `EPROTOTYPE
	| 63 -> `ERANGE
	| 64 -> `EROFS
	| 65 -> `ESHUTDOWN
	| 66 -> `ESPIPE
	| 67 -> `ESRCH
	| 68 -> `ETIMEDOUT
	| 69 -> `ETXTBSY
	| 70 -> `EXDEV
	| 71 -> `UNKNOWN
	| 72 -> `EOF
	| 73 -> `ENXIO
	| 74 -> `EMLINK
	| 75 -> `ENOTTY
	| 76 -> `EFTYPE
	| 77 -> `EILSEQ
	| _ -> unexpected_value v "eval.luv.UVError"

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

let encode_callback encode_result v_callback result =
	let cb = prepare_callback v_callback 1 in
	ignore(cb [encode_result result])

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
	| HIdle t -> Handle.coerce t
	| HTimer t -> Handle.coerce t
	| HAsync t -> Handle.coerce t
	| _ -> unexpected_value v "eval.luv.Handle"

let decode_idle v =
	match decode_handle v with
	| HIdle t -> t
	| _ -> unexpected_value v "eval.luv.Idle"

let decode_timer v =
	match decode_handle v with
	| HTimer t -> t
	| _ -> unexpected_value v "eval.luv.Timer"

let decode_async v =
	match decode_handle v with
	| HAsync t -> t
	| _ -> unexpected_value v "eval.luv.Async"

let decode_buffer v =
	match decode_handle v with
	| HBuffer t -> t
	| _ -> unexpected_value v "eval.luv.Buffer"

let decode_sockaddr v =
	match decode_handle v with
	| HSockAddr t -> t
	| _ -> unexpected_value v "eval.luv.SockAddr"

let encode_sockaddr h =
	VHandle (HSockAddr h)

let decode_tcp v =
	match decode_handle v with
	| HTcp t -> t
	| _ -> unexpected_value v "eval.luv.Tcp"

let decode_socket_type v : Sockaddr.Socket_type.t =
	match decode_enum v with
	| 0, [] -> `STREAM
	| 1, [] -> `DGRAM
	| 2, [] -> `RAW
	| 3, [v] -> `OTHER (decode_int v)
	| _ -> unexpected_value v "eval.luv.SockAddr.SocketType"

let decode_address_family v : Sockaddr.Address_family.t =
	match decode_enum v with
	| 0, [] -> `UNSPEC
	| 1, [] -> `INET
	| 2, [] -> `INET6
	| 3, [v] -> `OTHER (decode_int v)
	| _ -> unexpected_value v "eval.luv.SockAddr.AddressType"

let decode_pipe v =
	match decode_handle v with
	| HPipe t -> t
	| _ -> unexpected_value v "eval.luv.Pipe"

let uv_error_fields = [
	"toString", vfun1 (fun v ->
		let e = decode_uv_error v in
		EvalString.create_unknown (Error.strerror e)
	);
	"errName", vfun1 (fun v ->
		let e = decode_uv_error v in
		EvalString.create_unknown (Error.err_name e)
	);
	"translateSysError", vfun1 (fun v ->
		let e = decode_int v in
		encode_uv_error (Error.translate_sys_error e)
	);
	"setOnUnhandledException", vfun1 (fun v ->
		let cb = prepare_callback v 1 in
		Error.set_on_unhandled_exception (fun ex ->
			let msg =
				match ex with
				| HaxeError.Error (Custom msg,_) ->
					(* Eval interpreter rethrows runtime exceptions as `Custom "Exception message\nException stack"` *)
					(try fst (ExtString.String.split msg "\n")
					with _ -> msg)
				| HaxeError.Error (err,_) -> HaxeError.error_msg err
				| _ -> Printexc.to_string ex
			in
			let e = create_haxe_exception ~stack:(get_ctx()).exception_stack msg in
			ignore(cb [e])
		);
		vnull
	);
]

let loop_fields = [
	"run", vfun2 (fun v1 v2 ->
		let loop = decode_loop_opt v1
		and mode =
			match decode_int v2 with
			| 0 -> `DEFAULT
			| 1 -> `ONCE
			| 2 -> `NOWAIT
			| _ -> unexpected_value v2 "valid loop run mode"
		in
		vbool (Loop.run ~loop ~mode ())
	);
	"stop", vfun1 (fun v ->
		let loop = decode_loop v in
		Loop.stop loop;
		vnull
	);
	"init", vfun0 (fun () ->
		encode_result (fun l -> VHandle (HLoop l)) (Loop.init())
	);
	"close", vfun1 (fun v ->
		let loop = decode_loop v in
		encode_unit_result (Loop.close loop)
	);
	"alive", vfun1 (fun v ->
		let loop = decode_loop v in
		vbool (Loop.alive loop)
	);
	"defaultLoop", vfun0 (fun () ->
		VHandle (HLoop (Loop.default()))
	);
	"libraryShutdown", vfun0 (fun () ->
		Loop.library_shutdown();
		vnull
	);
	"now", vfun1 (fun v ->
		let loop = decode_loop v in
		VUInt64 (Loop.now loop)
	);
	"updateTime", vfun1 (fun v ->
		let loop = decode_loop v in
		Loop.update_time loop;
		vnull
	);
]

let handle_fields = [
	"close", vfun2 (fun v1 v2 ->
		let handle = decode_luv_handle v1
		and cb = prepare_callback v2 0 in
		Handle.close handle (fun() -> ignore(cb []));
		vnull
	);
	"isActive", vfun1 (fun v ->
		let handle = decode_luv_handle v in
		vbool (Handle.is_active handle)
	);
	"isClosing", vfun1 (fun v ->
		let handle = decode_luv_handle v in
		vbool (Handle.is_closing handle)
	);
	"ref", vfun1 (fun v ->
		let handle = decode_luv_handle v in
		Handle.ref handle;
		vnull
	);
	"unref", vfun1 (fun v ->
		let handle = decode_luv_handle v in
		Handle.unref handle;
		vnull
	);
	"hasRef", vfun1 (fun v ->
		let handle = decode_luv_handle v in
		vbool (Handle.has_ref handle)
	);
]

let idle_fields = [
	"init", vfun1 (fun v ->
		let loop = decode_loop_opt v in
		encode_result (fun i -> VHandle (HIdle i)) (Idle.init ~loop ())
	);
	"start", vfun2 (fun v1 v2 ->
		let idle = decode_idle v1 in
		let cb = prepare_callback v2 0 in
		encode_unit_result (Idle.start idle (fun() -> ignore(cb [])));
	);
	"stop", vfun1 (fun v ->
		let idle = decode_idle v in
		encode_unit_result (Idle.stop idle)
	);
]

let timer_fields = [
	"init", vfun1 (fun v ->
		let loop = decode_loop_opt v in
		encode_result (fun i -> VHandle (HTimer i)) (Timer.init ~loop ())
	);
	"start", vfun4 (fun v1 v2 v3 v4 ->
		let timer = decode_timer v1
		and cb = prepare_callback v2 0
		and timeout = decode_int v3
		and repeat = default_int v4 0 in
		encode_unit_result (Timer.start ~repeat timer timeout (fun() -> ignore(cb [])));
	);
	"stop", vfun1 (fun v ->
		let timer = decode_timer v in
		encode_unit_result (Timer.stop timer)
	);
	"again", vfun1 (fun v ->
		let timer = decode_timer v in
		encode_unit_result (Timer.again timer)
	);
	"set_repeat", vfun2 (fun v1 v2 ->
		let timer = decode_timer v1
		and repeat = decode_int v2 in
		Timer.set_repeat timer repeat;
		vint repeat
	);
	"get_repeat", vfun1 (fun v1 ->
		let timer = decode_timer v1 in
		vint (Timer.get_repeat timer)
	);
	"get_dueIn", vfun1 (fun v1 ->
		let timer = decode_timer v1 in
		vint (Timer.get_due_in timer)
	);
]

let async_fields = [
	"init", vfun2 (fun v1 v2 ->
		let loop = decode_loop_opt v1
		and cb = prepare_callback v2 1 in
		let callback async = ignore(cb [VHandle (HAsync async)]) in
		encode_result (fun i -> VHandle (HAsync i)) (Async.init ~loop callback)
	);
	"send", vfun1 (fun v ->
		let async = decode_async v in
		encode_unit_result (Async.send async);
	);
]

let buffer_get getter = vfun2 (fun v1 v2 ->
	let buffer = decode_buffer v1
	and index = decode_int v2 in
	vint (int_of_char (getter buffer index))
)

let buffer_set setter = vfun3 (fun v1 v2 v3 ->
	let buffer = decode_buffer v1
	and index = decode_int v2
	and byte = decode_int v3 in
	setter buffer index (char_of_int byte);
	v3
)

let buffer_fields = [
	"create", vfun1 (fun v ->
		let size = decode_int v in
		VHandle (HBuffer (Buffer.create size))
	);
	"fromNativeString", vfun1 (fun v ->
		let s = decode_native_string v in
		VHandle (HBuffer (Buffer.from_string s))
	);
	"fromString", vfun1 (fun v ->
		let s = decode_string v in
		VHandle (HBuffer (Buffer.from_string s))
	);
	"fromBytes", vfun1 (fun v ->
		let b = decode_bytes v in
		VHandle (HBuffer (Buffer.from_bytes b))
	);
	"totalSize", vfun1 (fun v ->
		let l = List.map decode_buffer (decode_array v) in
		vint (Buffer.total_size l)
	);
	"drop", vfun2 (fun v1 v2 ->
		let l = List.map decode_buffer (decode_array v1)
		and count = decode_int v2
		and encode_buffer buffer = VHandle (HBuffer buffer) in
		encode_array (List.map encode_buffer (Buffer.drop l count))
	);
	"size", vfun1 (fun v ->
		let buffer = decode_buffer v in
		vint (Buffer.size buffer)
	);
	"get", buffer_get Buffer.get;
	"unsafeGet", buffer_get Buffer.unsafe_get;
	"set", buffer_set Buffer.set;
	"unsafeSet", buffer_set Buffer.unsafe_set;
	"sub", vfun3 (fun v1 v2 v3 ->
		let buffer = decode_buffer v1
		and offset = decode_int v2
		and length = decode_int v3 in
		VHandle (HBuffer (Buffer.sub buffer offset length))
	);
	"blit", vfun2 (fun v1 v2 ->
		let buffer = decode_buffer v1
		and destination = decode_buffer v2 in
		Buffer.blit buffer destination;
		vnull
	);
	"fill", vfun2 (fun v1 v2 ->
		let buffer = decode_buffer v1
		and byte = decode_int v2 in
		Buffer.fill buffer (char_of_int byte);
		vnull
	);
	"toString", vfun1 (fun v ->
		let buffer = decode_buffer v in
		EvalString.create_unknown (Buffer.to_string buffer)
	);
	"toNativeString", vfun1 (fun v ->
		let buffer = decode_buffer v in
		vnative_string (Buffer.to_string buffer)
	);
	"toBytes", vfun1 (fun v ->
		let buffer = decode_buffer v in
		encode_bytes (Buffer.to_bytes buffer)
	);
	"blitToBytes", vfun3 (fun v1 v2 v3 ->
		let buffer = decode_buffer v1
		and destination = decode_bytes v2
		and offset = decode_int v3 in
		Buffer.blit_to_bytes buffer destination offset;
		vnull
	);
	"blitFromBytes", vfun3 (fun v1 v2 v3 ->
		let buffer = decode_buffer v1
		and source = decode_bytes v2
		and offset = decode_int v3 in
		Buffer.blit_from_bytes buffer source offset;
		vnull
	);
	"blitFromBytes", vfun3 (fun v1 v2 v3 ->
		let buffer = decode_buffer v1
		and source = decode_native_string v2
		and offset = decode_int v3 in
		Buffer.blit_from_string buffer source offset;
		vnull
	);
]

let sockaddr_fields = [
	"get_port", vfun1 (fun v ->
		let a = decode_sockaddr v in
		encode_option vint (Sockaddr.port a)
	);
	"ipv4", vfun2 (fun v1 v2 ->
		let host = decode_string v1
		and port = decode_int v2 in
		encode_result encode_sockaddr (Sockaddr.ipv4 host port)
	);
	"ipv6", vfun2 (fun v1 v2 ->
		let host = decode_string v1
		and port = decode_int v2 in
		encode_result encode_sockaddr (Sockaddr.ipv6 host port)
	);
	"toString", vfun1 (fun v ->
		let a = decode_sockaddr v in
		match Sockaddr.to_string a with
		| Some s -> EvalString.create_unknown s
		| None -> EvalString.vstring (EvalString.create_ascii "")
	);
]

let tcp_fields = [
	"init", vfun2 (fun v1 v2 ->
		let loop = decode_loop_opt v1 in
		let tcp =
			if v2 = VNull then
				TCP.init ~loop ()
			else
				let domain = decode_address_family v2 in
				TCP.init ~loop ~domain ()
		in
		encode_result (fun t -> VHandle (HTcp t)) tcp
	);
	"noDelay", vfun2 (fun v1 v2 ->
		let tcp = decode_tcp v1
		and value = decode_bool v2 in
		encode_unit_result (TCP.nodelay tcp value)
	);
	"keepAlive", vfun2 (fun v1 v2 ->
		let tcp = decode_tcp v1
		and value = decode_option decode_int v2 in
		encode_unit_result (TCP.keepalive tcp value)
	);
	"simultaneousAccepts", vfun2 (fun v1 v2 ->
		let tcp = decode_tcp v1
		and value = decode_bool v2 in
		encode_unit_result (TCP.simultaneous_accepts tcp value)
	);
	"bind", vfun3 (fun v1 v2 v3 ->
		let tcp = decode_tcp v1
		and addr = decode_sockaddr v2
		and ipv6only = if v3 = VNull then false else decode_bool v3 in
		encode_unit_result (TCP.bind ~ipv6only tcp addr)
	);
	"getSockName", vfun1 (fun v ->
		let tcp = decode_tcp v in
		encode_result encode_sockaddr (TCP.getsockname tcp)
	);
	"getPeerName", vfun1 (fun v ->
		let tcp = decode_tcp v in
		encode_result encode_sockaddr (TCP.getpeername tcp)
	);
	"connect", vfun3 (fun v1 v2 v3 ->
		let tcp = decode_tcp v1
		and addr = decode_sockaddr v2 in
		TCP.connect tcp addr (encode_callback encode_unit_result v3);
		vnull
	);
	"closeReset", vfun2 (fun v1 v2 ->
		let tcp = decode_tcp v1 in
		TCP.close_reset tcp (encode_callback encode_unit_result v2);
		vnull
	);
]

let pipe_fields = [
	"init", vfun2 (fun v1 v2 ->
		let loop = decode_loop_opt v1
		and for_handle_passing = if v2 = VNull then false else decode_bool v2 in
		encode_result (fun p -> VHandle (HPipe p)) (Pipe.init ~loop ~for_handle_passing ())
	);
	"bind", vfun2 (fun v1 v2 ->
		let pipe = decode_pipe v1
		and name = decode_native_string v2 in
		encode_unit_result (Pipe.bind pipe name)
	);
	"connect", vfun3 (fun v1 v2 v3 ->
		let pipe = decode_pipe v1
		and target = decode_native_string v2 in
		Pipe.connect pipe target (encode_callback encode_unit_result v3);
		vnull
	);
	"getSockName", vfun1 (fun v ->
		let pipe = decode_pipe v in
		encode_result vnative_string (Pipe.getsockname pipe)
	);
	"getPeerName", vfun1 (fun v ->
		let pipe = decode_pipe v in
		encode_result vnative_string (Pipe.getpeername pipe)
	);
	"pendingInstances", vfun2 (fun v1 v2 ->
		let pipe = decode_pipe v1
		and amount = decode_int v2 in
		Pipe.pending_instances pipe amount;
		vnull
	);
	"receiveHandle", vfun1 (fun v ->
		let pipe = decode_pipe v in
		let index,args =
			match Pipe.receive_handle pipe with
			| `None ->
				0,[||]
			| `TCP assoc ->
				1,[|vfun1 (fun v -> encode_unit_result (assoc (decode_tcp v)))|]
			| `Pipe assoc ->
				2,[|vfun1 (fun v -> encode_unit_result (assoc (decode_pipe v)))|]
		in
		encode_enum_value key_eval_luv_ReceiveHandle index args None
	);
	"chmod", vfun2 (fun v1 v2 ->
		let pipe = decode_pipe v1
		and mode =
			match decode_int v2 with
			| 0 -> [`READABLE]
			| 1 -> [`WRITABLE]
			| 2 -> [`READABLE; `WRITABLE]
			| _ -> unexpected_value v2 "eval.luv.Pipe.PipeMode"
		in
		encode_unit_result (Pipe.chmod pipe mode)
	);
]