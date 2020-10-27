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
open EvalField
open EvalIntegers

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

let encode_callback encode_ok_value v_callback result =
	let cb = prepare_callback v_callback 1 in
	ignore(cb [encode_result encode_ok_value result])

let encode_unit () =
	vnull

let encode_unit_result =
	encode_result encode_unit

let encode_unit_callback =
	encode_callback encode_unit

let resolve_result = function
	| Ok v -> v
	| Error e -> throw (luv_exception e) null_pos

let decode_loop v =
	match decode_handle v with
	| HLoop t -> t
	| _ -> unexpected_value v "eval.luv.Loop"

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
	| HProcess t -> Handle.coerce t
	(* TODO
	| HCheck t -> Handle.coerce t
	| HFS_event t -> Handle.coerce t
	| HFS_poll t -> Handle.coerce t
	| HPoll t -> Handle.coerce t
	| HPrepare t -> Handle.coerce t
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

let encode_address_family (a:Sockaddr.Address_family.t) =
	let index,args =
		match a with
		| `UNSPEC -> 0, [||]
		| `INET -> 1, [||]
		| `INET6 -> 2, [||]
		| `OTHER i -> 3, [|vint i|]
	in
	encode_enum_value key_eval_luv_AddressFamily index args None

let encode_socket_type (a:Sockaddr.Socket_type.t) =
	let index,args =
		match a with
		| `STREAM -> 0, [||]
		| `DGRAM -> 1, [||]
		| `RAW -> 2, [||]
		| `OTHER i -> 3, [|vint i|]
	in
	encode_enum_value key_eval_luv_SocketType index args None

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

let decode_process v =
	match decode_handle v with
	| HProcess t -> t
	| _ -> unexpected_value v "eval.luv.Process"

let decode_file_mode v : File.Mode.t =
	match decode_enum v with
	| 0,[] -> `IRWXU
	| 1,[] -> `IRUSR
	| 2,[] -> `IWUSR
	| 3,[] -> `IXUSR
	| 4,[] -> `IRWXG
	| 5,[] -> `IRGRP
	| 6,[] -> `IWGRP
	| 7,[] -> `IXGRP
	| 8,[] -> `IRWXO
	| 9,[] -> `IROTH
	| 10,[] -> `IWOTH
	| 11,[] -> `IXOTH
	| 12,[] -> `ISUID
	| 13,[] -> `ISGID
	| 14,[] -> `ISVTX
	| 15,[] -> `IFMT
	| 16,[] -> `IFREG
	| 17,[] -> `IFDIR
	| 18,[] -> `IFBLK
	| 19,[] -> `IFCHR
	| 20,[] -> `IFLNK
	| 21,[] -> `IFIFO
	| 22,[v2] -> `NUMERIC (decode_int v2)
	| _ -> unexpected_value v "eval.luv.File.FileMode"

let decode_file_mode_list v =
	List.map decode_file_mode (decode_array v)

let decode_file_request v =
	match v with
	| VHandle (HFileRequest r) -> r
	| _ -> unexpected_value v "eval.luv.File.FileRequest"

let encode_timespec (t:File.Stat.timespec) =
	encode_obj [
		key_sec, VInt64 (Signed.Long.to_int64 t.sec);
		key_nsec, VInt64 (Signed.Long.to_int64 t.nsec)
	]

let decode_dir v =
	match v with
	| VHandle (HDir dir) -> dir
	| _ -> unexpected_value v "eval.luv.Dir"

let encode_dirent (de:File.Dirent.t) =
	let kind =
		match de.kind with
		| `UNKNOWN -> 0
		| `FILE -> 1
		| `DIR -> 2
		| `LINK -> 3
		| `FIFO -> 4
		| `SOCKET -> 5
		| `CHAR -> 6
		| `BLOCK -> 7
	in
	encode_obj [key_kind,vint kind; key_name,vnative_string de.name]

let encode_scandir sd =
	encode_obj [
		key_next,vfun0 (fun() -> encode_nullable encode_dirent (File.scandir_next sd));
		key_end,vfun0 (fun() -> File.scandir_end sd; vnull);
	]

let decode_file_open_flag v : File.Open_flag.t =
	match decode_int v with
	| 0 -> `RDONLY
	| 1 -> `WRONLY
	| 2 -> `RDWR
	| 3 -> `CREAT
	| 4 -> `EXCL
	| 5 -> `EXLOCK
	| 6 -> `NOCTTY
	| 7 -> `NOFOLLOW
	| 8 -> `TEMPORARY
	| 9 -> `TRUNC
	| 10 -> `APPEND
	| 11 -> `DIRECT
	| 12 -> `DSYNC
	| 13 -> `FILEMAP
	| 14 -> `NOATIME
	| 15 -> `NONBLOCK
	| 16 -> `RANDOM
	| 17 -> `SEQUENTIAL
	| 18 -> `SHORT_LIVED
	| 19 -> `SYMLINK
	| 20 -> `SYNC
	| _ -> unexpected_value v "eval.luv.File.FileOpenFlag"

let encode_file_stat (s:File.Stat.t) =
	encode_obj [
		key_dev,VUInt64 s.dev;
		key_mode, VHandle (HFileModeNumeric s.mode);
		key_nlink,VUInt64 s.nlink;
		key_uid,VUInt64 s.uid;
		key_gid,VUInt64 s.gid;
		key_rdev,VUInt64 s.rdev;
		key_ino,VUInt64 s.ino;
		key_size,VUInt64 s.size;
		key_blksize,VUInt64 s.blksize;
		key_blocks,VUInt64 s.blocks;
		key_flags,VUInt64 s.flags;
		key_gen,VUInt64 s.gen;
		key_atim,encode_timespec s.atim;
		key_mtim,encode_timespec s.mtim;
		key_ctim,encode_timespec s.ctim;
		key_birthtim,encode_timespec s.birthtim;
	]

let encode_file_statfs (s:File.Statfs.t) =
	encode_obj [
		key_type, VUInt64 s.type_;
		key_bsize, VUInt64 s.bsize;
		key_blocks, VUInt64 s.blocks;
		key_bfree, VUInt64 s.bfree;
		key_bavail, VUInt64 s.bavail;
		key_files, VUInt64 s.files;
		key_ffree, VUInt64 s.ffree;
		key_fspare, match s.f_spare with u1, u2, u3, u4 -> encode_array [VUInt64 u1; VUInt64 u2; VUInt64 u3; VUInt64 u4]
	]

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
		let loop = decode_loop v1
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
		let loop = decode_loop v in
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
		let loop = decode_loop v in
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
		let loop = decode_loop v1
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
		let loop = decode_loop v1
		and domain = decode_optional decode_address_family v2 in
		let tcp = TCP.init ~loop ?domain () in
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
		TCP.connect tcp addr (encode_unit_callback v3);
		vnull
	);
	"closeReset", vfun2 (fun v1 v2 ->
		let tcp = decode_tcp v1 in
		TCP.close_reset tcp (encode_unit_callback v2);
		vnull
	);
]

let udp_fields = [
	"init", vfun3 (fun v1 v2 v3 ->
		let loop = decode_loop v1
		and domain = decode_optional decode_address_family v2
		and recvmmsg = decode_nullable decode_bool false v3 in
		let udp = UDP.init ~loop ?domain ~recvmmsg () in
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
		UDP.send udp l addr (encode_unit_callback v4);
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
			encode_obj [
				key_data,encode_buffer buf;
				key_addr,encode_option encode_sockaddr addr;
				key_flags,encode_array (List.map encode_flag flags)
			]
		in
		let udp = decode_udp v1
		and callback = encode_callback encode v2
		and allocate =
			decode_optional (fun v ->
				let cb = prepare_callback v 1 in
				(fun i -> decode_buffer (cb [vint i]))
			) v3
		in
		UDP.recv_start ?allocate udp callback;
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
		UDP.Connected.send udp l (encode_unit_callback v3);
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
		let loop = decode_loop v1
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
		Pipe.connect pipe target (encode_unit_callback v3);
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
		let loop = decode_loop v1
		and file = decode_file v2 in
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
		let encode (w,h) = encode_obj [key_width,vint w; key_height,vint h] in
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
		Stream.shutdown stream (encode_unit_callback v2);
		vnull
	);
	"listen", vfun2 (fun v1 v2 ->
		let stream = decode_stream v1 in
		Stream.listen stream (encode_unit_callback v2);
		vnull
	);
	"accept", vfun2 (fun v1 v2 ->
		let server = decode_stream v1
		and client = decode_stream v2 in
		encode_unit_result (Stream.accept server client)
	);
	"readStart", vfun3 (fun v1 v2 v3 ->
		let stream = decode_stream v1
		and callback = encode_callback encode_buffer v2
		and allocate =
			decode_optional (fun v ->
				let cb = prepare_callback v 1 in
				(fun i -> decode_buffer (cb [vint i]))
			) v3
		in
		Stream.read_start ?allocate stream callback;
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
		let loop = decode_loop v in
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

let process_fields = [
	"stdin", vint Process.stdin;
	"stdout", vint Process.stdout;
	"stderr", vint Process.stderr;
	"toParentPipe", vfun5 (fun v1 v2 v3 v4 v5 ->
		let fd = decode_int v1
		and parent_pipe = decode_pipe v2
		and readable_in_child = decode_bool v3
		and writable_in_child = decode_bool v4
		and overlapped = decode_bool v5 in
		let r = Process.to_parent_pipe ~fd ~parent_pipe ~readable_in_child ~writable_in_child ~overlapped () in
		VHandle (HRedirection r)
	);
	"inheritFd", vfun2 (fun v1 v2 ->
		let fd = decode_int v1
		and from_parent_fd = decode_int v2 in
		let r = Process.inherit_fd ~fd ~from_parent_fd () in
		VHandle (HRedirection r)
	);
	"inheritStream", vfun2 (fun v1 v2 ->
		let fd = decode_int v1
		and from_parent_stream = decode_stream v2 in
		let r = Process.inherit_stream ~fd ~from_parent_stream () in
		VHandle (HRedirection r)
	);
	"spawn", vfun4 (fun v1 v2 v3 v4 ->
		let loop = decode_loop v1
		and cmd = decode_string v2
		and args = List.map decode_string (decode_array v3) in
		let result =
			if v4 = VNull then
				Process.spawn ~loop cmd args
			else begin
				let options = decode_object v4 in
				let get name_hash f =
					let v = object_field options name_hash in
					decode_optional f v
				in
				let on_exit =
					get key_onExit (fun v ->
						let cb = prepare_callback v 3 in
						(fun p ~exit_status ~term_signal ->
							ignore(cb [VHandle (HProcess p); VInt64 exit_status; vint term_signal])
						)
					)
				and environment =
					get key_environment (fun v ->
						match decode_instance v with
						| { ikind = IStringMap m } ->
							StringHashtbl.fold (fun k (_,v) acc -> (k, decode_string v) :: acc) m []
						| _ ->
							unexpected_value v "haxe.ds.Map<String,String>"
					)
				and redirect =
					get key_redirect (fun v ->
						List.map (fun v2 ->
							match v2 with
							| VHandle (HRedirection r) -> r
							| _ -> unexpected_value v2 "eval.luv.Process.Redirection"
						) (decode_array v)
					)
				and working_directory = get key_workingDirectory decode_string
				and uid = get key_uid decode_int
				and gid = get key_gid decode_int
				and windows_verbatim_arguments = get key_windowsVerbatimArguments decode_bool
				and detached = get key_detached decode_bool
				and windows_hide = get key_windowsHide decode_bool
				and windows_hide_console = get key_windowsHideConsole decode_bool
				and windows_hide_gui = get key_windowsHideGui decode_bool
				in
				(* Process.spawn ~loop ?detached cmd args *)
				Process.spawn ~loop ?on_exit ?environment ?working_directory ?redirect
					?uid ?gid ?windows_verbatim_arguments ?detached ?windows_hide
					?windows_hide_console ?windows_hide_gui cmd args
			end
		in
		encode_result (fun p -> VHandle (HProcess p)) result
	);
	"disableStdioInheritance", vfun0 (fun() ->
		Process.disable_stdio_inheritance();
		vnull
	);
	"killPid", vfun2 (fun v1 v2 ->
		let pid = decode_int v1
		and sig_num = decode_int v2 in
		encode_unit_result (Process.kill_pid ~pid sig_num)
	);
	"pid", vfun1 (fun v ->
		let p = decode_process v in
		vint (Process.pid p)
	);
]

let request_fields = [
	"cancel", vfun1 (fun v ->
		encode_unit_result (match v with
			| VHandle (HFileRequest r) -> Request.cancel r
			| VHandle (HAddrRequest r) -> Request.cancel r
			| VHandle (HNameRequest r) -> Request.cancel r
			| VHandle (HRandomRequest r) -> Request.cancel r
			| VHandle (HThreadPoolRequest r) -> Request.cancel r
			| _ -> unexpected_value v "eval.luv.Request"
		)
	)
]

let dns_fields = [
	"createAddrRequest", vfun0 (fun () ->
		VHandle (HAddrRequest (DNS.Addr_info.Request.make()))
	);
	"createInfoRequest", vfun0 (fun () ->
		VHandle (HNameRequest (DNS.Name_info.Request.make()))
	);
	"getAddrInfo", vfun5 (fun v1 v2 v3 v4 v5 ->
		let loop = decode_loop v1
		and node = decode_nullable (fun v -> Some (decode_string v)) None v2
		and service = decode_nullable (fun v -> Some (decode_string v)) None v3
		in
		if node = None && service = None then
			throw (create_haxe_exception "Either node or service has to be not null") null_pos
		else begin
			let callback =
				let cb = prepare_callback v5 1 in
				(fun result ->
					let v =
						encode_result (fun infos ->
							encode_array (List.map (fun (info:DNS.Addr_info.t) ->
								let fields = [
									key_family,encode_address_family info.family;
									key_sockType,encode_socket_type info.socktype;
									key_protocol,vint info.protocol;
									key_addr,encode_sockaddr info.addr;
								] in
								let fields =
									match info.canonname with
									| None -> fields
									| Some s -> (key_canonName,EvalString.create_unknown s) :: fields
								in
								encode_obj fields
							) infos)
						) result
					in
					ignore(cb [v])
				)
			in
			if v4 = VNull then
				DNS.getaddrinfo ~loop ?node ?service () callback
			else begin
				let options = decode_object v4 in
				let get name_hash f =
					let v = object_field options name_hash in
					decode_optional f v
				in
				let request =
					get key_request (function
						| VHandle (HAddrRequest r) -> r
						| v -> unexpected_value v "eval.luv.Dns.AddrInfoRequest"
					)
				and family = get key_family decode_address_family
				and socktype = get key_sockType decode_socket_type
				and protocol = get key_protocol decode_int
				and flags =
					get key_flags (fun v ->
						List.map (fun v ->
							match decode_int v with
							| 0 -> `PASSIVE
							| 1 -> `CANONNAME
							| 2 -> `NUMERICHOST
							| 3 -> `NUMERICSERV
							| 4 -> `V4MAPPED
							| 5 -> `ALL
							| 6 -> `ADDRCONFIG
							| _ -> unexpected_value v "eval.luv.Dns.AddrInfoFlag"
						) (decode_array v)
					)
				in
				DNS.getaddrinfo ~loop ?request ?family ?socktype ?protocol ?flags ?service ?node () callback
			end;
			vnull
		end
	);
	"getNameInfo", vfun4 (fun v1 v2 v3 v4 ->
		let loop = decode_loop v1
		and addr = decode_sockaddr v2
		and callback =
			let cb = prepare_callback v4 1 in
			(fun result ->
				let v =
					encode_result (fun (node,service) ->
						encode_obj [
							key_node,encode_string node;
							key_service,encode_string service;
						]
					) result
				in
				ignore(cb [v])
			)
		in
		if v3 = VNull then
			DNS.getnameinfo ~loop addr callback
		else begin
			let options = decode_object v3 in
			let get name_hash f =
				let v = object_field options name_hash in
				decode_optional f v
			in
			let request =
				get key_request (function
					| VHandle (HNameRequest r) -> r
					| v -> unexpected_value v "eval.luv.Dns.NameInfoRequest"
				)
			and flags =
				get key_flags (fun v ->
					List.map (fun v ->
						match decode_int v with
						| 0 -> `NAMEREQD
						| 1 -> `DGRAM
						| 2 -> `NOFQDN
						| 3 -> `NUMERICHOST
						| 4 -> `NUMERICSERV
						| _ -> unexpected_value v "eval.luv.Dns.NameInfoFlag"
					) (decode_array v)
				)
			in
			DNS.getnameinfo ~loop ?request ?flags addr callback
		end;
		vnull
	);
]

module F = struct
	let async v1 v2 fn =
		let loop = Some (decode_loop v1)
		and request = decode_optional decode_file_request v2 in
		fn ?loop ?request

	let path v fn =
		fn (decode_native_string v)

	let file v fn =
		fn (decode_file v)

	let dir v fn =
		fn (decode_dir v)

	let to_ v fn =
		let to_ = decode_native_string v in
		fn ~to_

	let mode v fn =
		let mode = decode_file_mode_list v in
		fn mode

	let mode_opt v fn =
		let mode = decode_optional decode_file_mode_list v in
		fn ?mode

	let open_ v1 v2 v3 fn =
		let flags = List.map decode_file_open_flag (decode_array v3) in
		(fn |> mode_opt v1 |> path v2) flags

	let rename v1 v2 fn =
		fn |> path v1 |> to_ v2

	let mkdir v1 v2 fn =
		fn |> mode_opt v1 |> path v2

	let data v1 v2 v3 fn =
		let file = decode_file v1
		and file_offset = Some (decode_i64 v2)
		and buffers = decode_buffers v3 in
		fn ?file_offset file buffers

	let ftruncate v1 v2 fn =
		let file = decode_file v1
		and length = decode_i64 v2 in
		fn file length

	let copyFile v1 v2 v3 fn =
		let flags = List.map decode_int (decode_array v3) in
		let excl = if List.mem 0 flags then Some true else None
		and ficlone = if List.mem 1 flags then Some true else None
		and ficlone_force = if List.mem 2 flags then Some true else None in
		(fn ?excl ?ficlone ?ficlone_force) |> path v1 |> to_ v2

	let sendFile v1 v2 v3 v4 fn =
		let to_ = decode_file v2
		and offset = decode_i64 v3
		and length = decode_size_t v4 in
		(fn |> file v1) ~to_ ~offset length

	let access v1 v2 fn =
		let flags =
			List.map (fun v : File.Access_flag.t ->
				match decode_int v with
				| 0 -> `F_OK
				| 1 -> `R_OK
				| 2 -> `W_OK
				| 3 -> `X_OK
				| _ -> unexpected_value v "eval.luv.File.FileAccessFlag"
			) (decode_array v2) in
		(fn |> path v1) flags

	let utime v1 v2 fn =
		let atime = decode_float v1
		and mtime = decode_float v2 in
		fn ~atime ~mtime

	let link v fn =
		let link = decode_native_string v in
		fn ~link

	let symlink v fn =
		let flags = List.map decode_int (decode_array v) in
		let dir = if List.mem 0 flags then Some true else None
		and junction = if List.mem 1 flags then Some true else None in
		fn ?dir ?junction

	let chown v1 v2 fn =
		let uid = decode_int v1
		and gid = decode_int v2 in
		fn ~uid ~gid

	let readdir v1 v2 fn =
		let number_of_entries = decode_optional decode_int v2 in
		fn ?number_of_entries |> dir v1
end

let file_fields = [
	"get_stdin", VHandle (HFile File.stdin);
	"get_stdout", VHandle (HFile File.stdout);
	"get_stderr", VHandle (HFile File.stderr);
	"createRequest", vfun0 (fun() ->
		VHandle (HFileRequest (File.Request.make()))
	);
	"testMode", vfun2 (fun v1 v2 ->
		let mask = decode_file_mode_list v1
		and bits =
			match v2 with
			| VHandle (HFileModeNumeric m) -> m
			| _ -> unexpected_value v2 "eval.luv.File.FileModeNumeric"
		in
		vbool (File.Mode.test mask bits)
	);
	"open", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_callback (fun f -> VHandle (HFile f)) v6 in
		(File.open_ |> F.async v1 v5 |> F.open_ v4 v2 v3) callback;
		vnull
	);
	"close", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_unit_callback v4 in
		(File.close |> F.async v2 v3 |> F.file v1) callback;
		vnull
	);
	"read", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_callback encode_size_t v6 in
		(File.read |> F.async v2 v5 |> F.data v1 v3 v4) callback;
		vnull
	);
	"write", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_callback encode_size_t v6 in
		(File.write |> F.async v2 v5 |> F.data v1 v3 v4) callback;
		vnull
	);
	"unlink", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_unit_callback v4 in
		(File.unlink |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"rename", vfun5 (fun v1 v2 v3 v4 v5 ->
		let callback = encode_unit_callback v5 in
		(File.rename |> F.async v1 v4 |> F.rename v2 v3) callback;
		vnull
	);
	"mkstemp", vfun4 (fun v1 v2 v3 v4 ->
		let callback =
			encode_callback (fun (n,file) ->
				encode_obj [key_name,vnative_string n; key_file,VHandle (HFile file)]
			) v4
		in
		(File.mkstemp |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"mkdtemp", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback vnative_string v4 in
		(File.mkdtemp |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"mkdir", vfun5 (fun v1 v2 v3 v4 v5 ->
		let callback = encode_unit_callback v5 in
		(File.mkdir |> F.async v1 v4 |> F.mkdir v3 v2) callback;
		vnull
	);
	"rmdir", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_unit_callback v4 in
		(File.rmdir |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"stat", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback encode_file_stat v4 in
		(File.stat |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"lstat", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback encode_file_stat v4 in
		(File.lstat |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"fstat", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback encode_file_stat v4 in
		(File.fstat |> F.async v2 v3 |> F.file v1) callback;
		vnull
	);
	"statFs", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback encode_file_statfs v4 in
		(File.statfs |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"fsync", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_unit_callback v4 in
		(File.fsync |> F.async v2 v3 |> F.file v1) callback;
		vnull
	);
	"fdataSync", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_unit_callback v4 in
		(File.fdatasync |> F.async v2 v3 |> F.file v1) callback;
		vnull
	);
	"ftruncate", vfun5 (fun v1 v2 v3 v4 v5 ->
		let callback = encode_unit_callback v5 in
		(File.ftruncate |> F.async v2 v4 |> F.ftruncate v1 v3) callback;
		vnull
	);
	"copyFile", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_unit_callback v6 in
		(File.copyfile |> F.async v1 v5 |> F.copyFile v2 v3 v4) callback;
		vnull
	);
	"sendFile", vfun7 (fun v1 v2 v3 v4 v5 v6 v7 ->
		let callback = encode_callback encode_size_t v7 in
		(File.sendfile |> F.async v2 v6 |> F.sendFile v1 v3 v4 v5) callback;
		vnull
	);
	"access", vfun5 (fun v1 v2 v3 v4 v5 ->
		let callback = encode_unit_callback v5 in
		(File.access |> F.async v1 v4 |> F.access v2 v3) callback;
		vnull
	);
	"chmod", vfun5 (fun v1 v2 v3 v4 v5 ->
		let callback = encode_unit_callback v5 in
		(File.chmod |> F.async v1 v4 |> F.path v2 |> F.mode v3) callback;
		vnull
	);
	"fchmod", vfun5 (fun v1 v2 v3 v4 v5 ->
		let callback = encode_unit_callback v5 in
		(File.fchmod |> F.async v2 v4 |> F.file v1 |> F.mode v3) callback;
		vnull
	);
	"utime", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_unit_callback v6 in
		(File.utime |> F.async v1 v5 |> F.path v2 |> F.utime v3 v4) callback;
		vnull
	);
	"lutime", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_unit_callback v6 in
		(File.lutime |> F.async v1 v5 |> F.path v2 |> F.utime v3 v4) callback;
		vnull
	);
	"futime", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_unit_callback v6 in
		(File.futime |> F.async v2 v5 |> F.file v1 |> F.utime v3 v4) callback;
		vnull
	);
	"link", vfun5 (fun v1 v2 v3 v4 v5 ->
		let callback = encode_unit_callback v5 in
		(File.link |> F.async v1 v4 |> F.path v2 |> F.link v3) callback;
		vnull
	);
	"symlink", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_unit_callback v6 in
		(File.symlink |> F.async v1 v5 |> F.symlink v4 |> F.path v2 |> F.link v3) callback;
		vnull
	);
	"readLink", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback vnative_string v4 in
		(File.readlink |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"realPath", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback vnative_string v4 in
		(File.realpath |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"chown", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_unit_callback v6 in
		(File.chown |> F.async v1 v5 |> F.path v2 |> F.chown v3 v4) callback;
		vnull
	);
	"lchown", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_unit_callback v6 in
		(File.lchown |> F.async v1 v5 |> F.path v2 |> F.chown v3 v4) callback;
		vnull
	);
	"fchown", vfun6 (fun v1 v2 v3 v4 v5 v6 ->
		let callback = encode_unit_callback v6 in
		(File.fchown |> F.async v2 v5 |> F.file v1 |> F.chown v3 v4) callback;
		vnull
	);
	"toInt", vfun1 (fun v ->
		let file = decode_file v in
		vint (File.to_int file)
	);
]

let dir_fields = [
	"open", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback (fun dir -> VHandle (HDir dir)) v4 in
		(File.opendir |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
	"close", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_unit_callback v4 in
		(File.closedir |> F.async v2 v3 |> F.dir v1) callback;
		vnull
	);
	"read", vfun5 (fun v1 v2 v3 v4 v5 ->
		let callback =
			encode_callback (fun a ->
				encode_array_a (Array.map encode_dirent a)
			) v5
		in
		(File.readdir |> F.async v2 v4 |> F.readdir v1 v3) callback;
		vnull
	);
	"scan", vfun4 (fun v1 v2 v3 v4 ->
		let callback = encode_callback encode_scandir v4 in
		(File.scandir |> F.async v1 v3 |> F.path v2) callback;
		vnull
	);
]