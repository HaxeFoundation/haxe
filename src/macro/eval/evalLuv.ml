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
	| HPipe t -> Handle.coerce t
	| HTcp t -> Handle.coerce t
	| HTty t -> Handle.coerce t
	| HUdp t -> Handle.coerce t
	| HSignal t -> Handle.coerce t
	(* TODO
	| HCheck t -> Handle.coerce t
	| HFS_event t -> Handle.coerce t
	| HFS_poll t -> Handle.coerce t
	| HPoll t -> Handle.coerce t
	| HPrepare t -> Handle.coerce t
	| HProcess t -> Handle.coerce t
	*)
	| _ -> unexpected_value v "eval.luv.Handle"

let decode_socket_handle v : [< `Stream of [< `Pipe | `TCP ] | `UDP ] Luv.Handle.t =
	match decode_handle v with
	| HTcp t -> Obj.magic t
	| HUdp t -> Obj.magic t
	| HPipe t -> Obj.magic t
	| _ -> unexpected_value v "eval.luv.Handle.SocketHandle"

let decode_stream v : 'kind Luv.Stream.t =
	match decode_handle v with
	| HTcp t -> Stream.coerce t
	| HTty t -> Stream.coerce t
	| HPipe t -> Stream.coerce t
	| _ -> unexpected_value v "eval.luv.Stream"

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

let decode_buffers v =
	List.map decode_buffer (decode_array v)

let encode_buffer b =
	VHandle (HBuffer b)

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

let decode_udp v =
	match decode_handle v with
	| HUdp t -> t
	| _ -> unexpected_value v "eval.luv.Udp"

let encode_udp udp =
	VHandle (HUdp udp)

let decode_udp_membership v =
	match decode_int v with
	| 0 -> `LEAVE_GROUP
	| 1 -> `JOIN_GROUP
	| _ -> unexpected_value v "eval.luv.Udp.UdpMembership"

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

let decode_tty v =
	match decode_handle v with
	| HTty t -> t
	| _ -> unexpected_value v "eval.luv.Tty"

let decode_file v =
	match decode_handle v with
	| HFile f -> f
	| _ -> unexpected_value v "eval.luv.File"

let decode_signal v =
	match decode_handle v with
	| HSignal t -> t
	| _ -> unexpected_value v "eval.luv.Signal"

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
	"sendBufferSize", vfun1 (fun v ->
		let handle = decode_socket_handle v in
		encode_result vint (Handle.send_buffer_size handle)
	);
	"setSendBufferSize", vfun2 (fun v1 v2 ->
		let handle = decode_socket_handle v1
		and size = decode_int v2 in
		encode_unit_result (Handle.set_send_buffer_size handle size)
	);
	"recvBufferSize", vfun1 (fun v ->
		let handle = decode_socket_handle v in
		encode_result vint (Handle.recv_buffer_size handle)
	);
	"setRendBufferSize", vfun2 (fun v1 v2 ->
		let handle = decode_socket_handle v1
		and size = decode_int v2 in
		encode_unit_result (Handle.set_recv_buffer_size handle size)
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
		let cb = prepare_callback v1 1
		and loop = decode_loop_opt v2 in
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
		encode_buffer (Buffer.create size)
	);
	"fromNativeString", vfun1 (fun v ->
		let s = decode_native_string v in
		encode_buffer (Buffer.from_string s)
	);
	"fromString", vfun1 (fun v ->
		let s = decode_string v in
		encode_buffer (Buffer.from_string s)
	);
	"fromBytes", vfun1 (fun v ->
		let b = decode_bytes v in
		encode_buffer (Buffer.from_bytes b)
	);
	"totalSize", vfun1 (fun v ->
		let l = decode_buffers v in
		vint (Buffer.total_size l)
	);
	"drop", vfun2 (fun v1 v2 ->
		let l = decode_buffers v1
		and count = decode_int v2
		and encode_buffer buffer = encode_buffer buffer in
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
		encode_buffer (Buffer.sub buffer offset length)
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
		encode_nullable vint (Sockaddr.port a)
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
		and ipv6only = decode_nullable decode_bool false v3 in
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

let udp_fields = [
	"init", vfun3 (fun v1 v2 v3 ->
		let loop = decode_loop_opt v1
		and recvmmsg = decode_nullable decode_bool false v3 in
		let udp =
			if v2 = VNull then
				UDP.init ~loop ~recvmmsg ()
			else
				let domain = decode_address_family v2 in
				UDP.init ~loop ~domain ~recvmmsg ()
		in
		encode_result encode_udp udp
	);
	"bind", vfun4 (fun v1 v2 v3 v4 ->
		let udp = decode_udp v1
		and addr = decode_sockaddr v2
		and ipv6only = decode_nullable decode_bool false v3
		and reuseaddr = decode_nullable decode_bool false v4 in
		encode_unit_result (UDP.bind ~ipv6only ~reuseaddr udp addr)
	);
	"connect", vfun2 (fun v1 v2 ->
		let udp = decode_udp v1
		and addr = decode_sockaddr v2 in
		match UDP.Connected.connect udp addr with
		| Ok () -> encode_result encode_udp (Ok udp)
		| Error e -> encode_result encode_udp (Error e)
	);
	"getSockName", vfun1 (fun v ->
		let udp = decode_udp v in
		encode_result encode_sockaddr (UDP.getsockname udp)
	);
	"setMembership", vfun4 (fun v1 v2 v3 v4 ->
		let udp = decode_udp v1
		and group = decode_string v2
		and interface = decode_string v3
		and membership = decode_udp_membership v4 in
		encode_unit_result (UDP.set_membership udp ~group ~interface membership)
	);
	"setSourceMembership", vfun5 (fun v1 v2 v3 v4 v5 ->
		let udp = decode_udp v1
		and group = decode_string v2
		and interface = decode_string v3
		and source = decode_string v4
		and membership = decode_udp_membership v5 in
		encode_unit_result (UDP.set_source_membership udp ~group ~interface ~source membership)
	);
	"setMulticastLoop", vfun2 (fun v1 v2 ->
		let udp = decode_udp v1
		and value = decode_bool v2 in
		encode_unit_result (UDP.set_multicast_loop udp value)
	);
	"setMulticastTtl", vfun2 (fun v1 v2 ->
		let udp = decode_udp v1
		and value = decode_int v2 in
		encode_unit_result (UDP.set_multicast_ttl udp value)
	);
	"setMulticastInterface", vfun2 (fun v1 v2 ->
		let udp = decode_udp v1
		and value = decode_string v2 in
		encode_unit_result (UDP.set_multicast_interface udp value)
	);
	"setBroadcast", vfun2 (fun v1 v2 ->
		let udp = decode_udp v1
		and value = decode_bool v2 in
		encode_unit_result (UDP.set_broadcast udp value)
	);
	"setTtl", vfun2 (fun v1 v2 ->
		let udp = decode_udp v1
		and value = decode_int v2 in
		encode_unit_result (UDP.set_ttl udp value)
	);
	"send", vfun4 (fun v1 v2 v3 v4 ->
		let udp = decode_udp v1
		and l = decode_buffers v2
		and addr = decode_sockaddr v3 in
		UDP.send udp l addr (encode_callback encode_unit_result v4);
		vnull
	);
	"trySend", vfun3 (fun v1 v2 v3 ->
		let udp = decode_udp v1
		and l = decode_buffers v2
		and addr = decode_sockaddr v3 in
		encode_unit_result (UDP.try_send udp l addr)
	);
	"recvStart", vfun3 (fun v1 v2 v3 ->
		let encode (buf,addr,flags) =
			let encode_flag = function
				| `PARTIAL -> vint 0
				| `MMSG_CHUNK -> vint 1
				| `MMSG_FREE -> vint 2
			in
			encode_obj_s [
				"data",encode_buffer buf;
				"addr",encode_option encode_sockaddr addr;
				"flags",encode_array (List.map encode_flag flags)
			]
		in
		let udp = decode_udp v1
		and callback = encode_callback (encode_result encode) v2 in
		if v3 = VNull then
			UDP.recv_start udp callback
		else begin
			let allocate =
				let cb = prepare_callback v3 1 in
				(fun i -> decode_buffer (cb [vint i]))
			in
			UDP.recv_start ~allocate udp callback
		end;
		vnull
	);
	"recvStop", vfun1 (fun v ->
		let udp = decode_udp v in
		encode_unit_result (UDP.recv_stop udp)
	);
	"getSendQueueSize", vfun1 (fun v ->
		let udp = decode_udp v in
		vint (UDP.get_send_queue_size udp)
	);
	"getSendQueueCount", vfun1 (fun v ->
		let udp = decode_udp v in
		vint (UDP.get_send_queue_count udp)
	);
]

let connected_udp_fields = [
	"disconnect", vfun1 (fun v ->
		let udp = decode_udp v in
		encode_unit_result (UDP.Connected.disconnect udp)
	);
	"getPeerName", vfun1 (fun v ->
		let udp = decode_udp v in
		encode_result encode_sockaddr (UDP.Connected.getpeername udp)
	);
	"send", vfun3 (fun v1 v2 v3 ->
		let udp = decode_udp v1
		and l = decode_buffers v2 in
		UDP.Connected.send udp l (encode_callback encode_unit_result v3);
		vnull
	);
	"send", vfun2 (fun v1 v2 ->
		let udp = decode_udp v1
		and l = decode_buffers v2 in
		encode_unit_result (UDP.Connected.try_send udp l)
	);
]

let pipe_fields = [
	"init", vfun2 (fun v1 v2 ->
		let loop = decode_loop_opt v1
		and for_handle_passing = decode_nullable decode_bool false v2 in
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

let tty_fields = [
	"init", vfun2 (fun v1 v2 ->
		let file = decode_file v1
		and loop = decode_loop_opt v2 in
		encode_result (fun tty -> VHandle (HTty tty)) (TTY.init ~loop file)
	);
	"setMode", vfun2 (fun v1 v2 ->
		let tty = decode_tty v1
		and mode =
			match decode_int v2 with
			| 0 -> `NORMAL
			| 1 -> `RAW
			| 2 -> `IO
			| _ -> unexpected_value v2 "eval.luv.Tty.TtyMode"
		in
		encode_unit_result (TTY.set_mode tty mode)
	);
	"resetMode", vfun0 (fun () ->
		encode_unit_result (TTY.reset_mode ())
	);
	"getWinSize", vfun1 (fun v ->
		let tty = decode_tty v in
		let encode (w,h) = encode_obj_s ["width",vint w; "height",vint h] in
		encode_result encode (TTY.get_winsize tty)
	);
	"setVTermState", vfun1 (fun v ->
		let state =
			match decode_int v with
			| 0 -> `SUPPORTED
			| 1 -> `UNSUPPORTED
			| _ -> unexpected_value v "eval.luv.Tty.VTermState"
		in
		TTY.set_vterm_state state;
		vnull
	);
	"getVTermState", vfun0 (fun () ->
		let encode state =
			vint (match state with
			| `SUPPORTED -> 0
			| `UNSUPPORTED -> 1)
		in
		encode_result encode (TTY.get_vterm_state())
	);
]

let stream_fields = [
	"shutdown", vfun2 (fun v1 v2 ->
		let stream = decode_stream v1 in
		Stream.shutdown stream (encode_callback encode_unit_result v2);
		vnull
	);
	"listen", vfun2 (fun v1 v2 ->
		let stream = decode_stream v1 in
		Stream.listen stream (encode_callback encode_unit_result v2);
		vnull
	);
	"accept", vfun2 (fun v1 v2 ->
		let server = decode_stream v1
		and client = decode_stream v2 in
		encode_unit_result (Stream.accept server client)
	);
	"readStart", vfun3 (fun v1 v2 v3 ->
		let stream = decode_stream v1
		and callback = encode_callback (encode_result encode_buffer) v2 in
		if v3 = VNull then
			Stream.read_start stream callback
		else begin
			let allocate =
				let cb = prepare_callback v3 1 in
				(fun i -> decode_buffer (cb [vint i]))
			in
			Stream.read_start ~allocate stream callback
		end;
		vnull
	);
	"readStop", vfun1 (fun v ->
		let stream = decode_stream v in
		encode_unit_result (Stream.read_stop stream)
	);
	"write", vfun3 (fun v1 v2 v3 ->
		let stream = decode_stream v1
		and data = decode_buffers v2
		and callback =
			let cb = prepare_callback v3 2 in
			(fun result bytes_written ->
				ignore(cb [encode_unit_result result; vint bytes_written])
			)
		in
		Stream.write stream data callback;
		vnull
	);
	"write2", vfun4 (fun v1 v2 v3 v4 ->
		let stream = decode_pipe v1
		and data = decode_buffers v2
		and callback =
			let cb = prepare_callback v4 2 in
			(fun result bytes_written ->
				ignore(cb [encode_unit_result result; vint bytes_written])
			)
		in
		(match decode_enum v3 with
		| 0,[vh] -> Stream.write2 stream data ~send_handle:(decode_tcp vh) callback
		| 1,[vh] -> Stream.write2 stream data ~send_handle:(decode_pipe vh) callback
		| _ -> unexpected_value v3 "eval.luv.Stream.SendHandle"
		);
		vnull
	);
	"tryWrite", vfun2 (fun v1 v2 ->
		let stream = decode_pipe v1
		and data = decode_buffers v2 in
		encode_result vint (Stream.try_write stream data)
	);
	"isReadable", vfun1 (fun v ->
		let stream = decode_pipe v in
		vbool (Stream.is_readable stream)
	);
	"isWritable", vfun1 (fun v ->
		let stream = decode_pipe v in
		vbool (Stream.is_writable stream)
	);
	"setBlocking", vfun2 (fun v1 v2 ->
		let stream = decode_pipe v1
		and block = decode_bool v2 in
		encode_unit_result (Stream.set_blocking stream block)
	);
]

let signum_fields = [
	"SIGABRT", vint Signal.sigabrt;
	"SIGFPE", vint Signal.sigfpe;
	"SIGHUP", vint Signal.sighup;
	"SIGILL", vint Signal.sigill;
	"SIGINT", vint Signal.sigint;
	"SIGKILL", vint Signal.sigkill;
	"SIGSEGV", vint Signal.sigsegv;
	"SIGTERM", vint Signal.sigterm;
	"SIGWINCH", vint Signal.sigwinch;
]

let signal_fields = [
	"init", vfun1 (fun v ->
		let loop = decode_loop_opt v in
		encode_result (fun s -> VHandle (HSignal s)) (Signal.init ~loop ())
	);
	"start", vfun3 (fun v1 v2 v3 ->
		let s = decode_signal v1
		and signum = decode_int v2
		and cb = prepare_callback v3 0 in
		encode_unit_result (Signal.start s signum (fun() -> ignore(cb [])))
	);
	"startOneshot", vfun3 (fun v1 v2 v3 ->
		let s = decode_signal v1
		and signum = decode_int v2
		and cb = prepare_callback v3 0 in
		encode_unit_result (Signal.start_oneshot s signum (fun() -> ignore(cb [])))
	);
	"stop", vfun1 (fun v ->
		let s = decode_signal v in
		encode_unit_result (Signal.stop s)
	);
	"signum", vfun1 (fun v ->
		let s = decode_signal v in
		vint (Signal.signum s)
	);
]