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
	| `EOVERFLOW -> 78
	| `ESOCKTNOSUPPORT -> 79
	| `ENODATA -> 80
	| `EUNATCH -> 81
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
	| 78 -> `EOVERFLOW
	| 79 -> `ESOCKTNOSUPPORT
	| 80 -> `ENODATA
	| 81 -> `EUNATCH
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
		| Result.Ok r -> 0, [|f r|]
		| Result.Error e -> 1, [|encode_uv_error e|]
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
	| Result.Ok v -> v
	| Result.Error e -> throw (luv_exception e) null_pos

let decode_loop = function
	| VHandle (HLoop t) -> t
	| v -> unexpected_value v "eval.luv.Loop"

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
	| HFsEvent t -> Handle.coerce t
	| HFsPoll t -> Handle.coerce t
	| HPrepare t -> Handle.coerce t
	| HCheck t -> Handle.coerce t
	(* TODO
	| HPoll t -> Handle.coerce t
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

let decode_idle = function
	| VHandle (HIdle t) -> t
	| v -> unexpected_value v "eval.luv.Idle"

let decode_timer = function
	| VHandle (HTimer t) -> t
	| v -> unexpected_value v "eval.luv.Timer"

let decode_async = function
	| VHandle (HAsync t) -> t
	| v -> unexpected_value v "eval.luv.Async"

let decode_buffer = function
	| VHandle (HBuffer t) -> t
	| v -> unexpected_value v "eval.luv.Buffer"

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

let decode_tcp = function
	| VHandle (HTcp t) -> t
	| v -> unexpected_value v "eval.luv.Tcp"

let decode_udp = function
	| VHandle (HUdp t) -> t
	| v -> unexpected_value v "eval.luv.Udp"

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

let decode_pipe = function
	| VHandle (HPipe t) -> t
	| v -> unexpected_value v "eval.luv.Pipe"

let decode_tty = function
	| VHandle (HTty t) -> t
	| v -> unexpected_value v "eval.luv.Tty"

let decode_file = function
	| VHandle (HFile f) -> f
	| v -> unexpected_value v "eval.luv.File"

let decode_signal = function
	| VHandle (HSignal t) -> t
	| v -> unexpected_value v "eval.luv.Signal"

let decode_process = function
	| VHandle (HProcess t) -> t
	| v -> unexpected_value v "eval.luv.Process"

let decode_prepare = function
	| VHandle (HPrepare t) -> t
	| v -> unexpected_value v "eval.luv.Prepare"

let decode_check = function
	| VHandle (HCheck t) -> t
	| v -> unexpected_value v "eval.luv.Check"

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

let decode_file_request = function
	| VHandle (HFileRequest r) -> r
	| v -> unexpected_value v "eval.luv.File.FileRequest"

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

let decode_int_flags v =
	if v = VNull then []
	else List.map decode_int (decode_array v)

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

let decode_fs_event = function
	| VHandle (HFsEvent e) -> e
	| v -> unexpected_value v "eval.luv.FsEvent"

let decode_mutex = function
	| VHandle (HMutex m) -> m
	| v -> unexpected_value v "eval.luv.Mutex"

let decode_rwlock = function
	| VHandle (HRwLock l) -> l
	| v -> unexpected_value v "eval.luv.RwLock"

let decode_semaphore = function
	| VHandle (HSemaphore s) -> s
	| v -> unexpected_value v "eval.luv.Semaphore"

let decode_condition = function
	| VHandle (HCondition c) -> c
	| v -> unexpected_value v "eval.luv.Condition"

let decode_barrier = function
	| VHandle (HBarrier b) -> b
	| v -> unexpected_value v "eval.luv.Barrier"

let decode_fs_poll = function
	| VHandle (HFsPoll p) -> p
	| v -> unexpected_value v "eval.luv.FsPoll"

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
				(* TODO beware of err_sub here *)
				| HaxeError.Error { err_message = Custom msg } ->
					(* Eval interpreter rethrows runtime exceptions as `Custom "Exception message\nException stack"` *)
					(try fst (ExtString.String.split msg "\n") with _ -> msg)
				| HaxeError.Error err ->
						let messages = ref [] in
						HaxeError.recurse_error (fun depth err ->
							let cm = make_compiler_message ~from_macro:err.err_from_macro (HaxeError.error_msg err.err_message) err.err_pos depth DKCompilerMessage Error in
							let ectx = MessageReporting.create_error_context false in
							match MessageReporting.compiler_message_string ectx cm with
								| None -> ()
								| Some str -> messages := str :: !messages
						) err;
						ExtLib.String.join "\n" (List.rev !messages)
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
	"blitFromString", vfun3 (fun v1 v2 v3 ->
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
		and ipv6only = decode_optional decode_bool v3 in
		encode_unit_result (TCP.bind ?ipv6only tcp addr)
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
		and recvmmsg = decode_optional decode_bool v3 in
		let udp = UDP.init ~loop ?domain ?recvmmsg () in
		encode_result encode_udp udp
	);
	"bind", vfun4 (fun v1 v2 v3 v4 ->
		let udp = decode_udp v1
		and addr = decode_sockaddr v2
		and ipv6only = decode_optional decode_bool v3
		and reuseaddr = decode_optional decode_bool v4 in
		encode_unit_result (UDP.bind ?ipv6only ?reuseaddr udp addr)
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
		and for_handle_passing = decode_optional decode_bool v2 in
		encode_result (fun p -> VHandle (HPipe p)) (Pipe.init ~loop ?for_handle_passing ())
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
	"listen", vfun3 (fun v1 v2 v3 ->
		let stream = decode_stream v1 in
		let backlog = decode_optional (fun v -> decode_int v) v3 in
		Stream.listen ?backlog stream (encode_unit_callback v2);
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
		let stream = decode_stream v1
		and data = decode_buffers v2 in
		encode_result vint (Stream.try_write stream data)
	);
	"isReadable", vfun1 (fun v ->
		let stream = decode_stream v in
		vbool (Stream.is_readable stream)
	);
	"isWritable", vfun1 (fun v ->
		let stream = decode_stream v in
		vbool (Stream.is_writable stream)
	);
	"setBlocking", vfun2 (fun v1 v2 ->
		let stream = decode_stream v1
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
		and cmd = decode_native_string v2
		and args = List.map decode_native_string (decode_array v3) in
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
							StringHashtbl.fold (fun k (_,v) acc -> (k, decode_native_string v) :: acc) m []
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
				and working_directory = get key_workingDirectory decode_native_string
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
		and node = decode_optional decode_string v2
		and service = decode_optional decode_string v3
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
	let async ~vloop ~vrequest fn =
		let loop = Some (decode_loop vloop)
		and request = decode_optional decode_file_request vrequest in
		fn ?loop ?request

	let path ~vpath fn =
		fn (decode_native_string vpath)

	let file ~vfile fn =
		fn (decode_file vfile)

	let dir ~vdir fn =
		fn (decode_dir vdir)

	let to_ ~vto fn =
		let to_ = decode_native_string vto in
		fn ~to_

	let mode ~vmode fn =
		fn (decode_file_mode_list vmode)

	let mode_opt ~vmode fn =
		let mode = decode_optional decode_file_mode_list vmode in
		fn ?mode

	let open_ ~vmode ~vpath ~vflags fn =
		let flags = List.map decode_file_open_flag (decode_array vflags) in
		(fn |> mode_opt ~vmode |> path ~vpath) flags

	let rename ~vpath ~vto fn =
		fn |> path ~vpath |> to_ ~vto

	let mkdir ~vmode ~vpath fn =
		fn |> mode_opt ~vmode |> path ~vpath

	let data ~vfile_offset ~vfile ~vbuffers fn =
		let file = decode_file vfile
		and file_offset = Some (decode_i64 vfile_offset)
		and buffers = decode_buffers vbuffers in
		fn ?file_offset file buffers

	let ftruncate ~vfile ~vlength fn =
		let file = decode_file vfile
		and length = decode_i64 vlength in
		fn file length

	let copyFile ~vflags ~vpath ~vto fn =
		let flags = decode_int_flags vflags in
		let excl = if List.mem 0 flags then Some true else None
		and ficlone = if List.mem 1 flags then Some true else None
		and ficlone_force = if List.mem 2 flags then Some true else None in
		(fn ?excl ?ficlone ?ficlone_force) |> path ~vpath |> to_ ~vto

	let sendFile ~vfile ~vto ~voffset ~vlength fn =
		let to_ = decode_file vto
		and offset = decode_i64 voffset
		and length = decode_size_t vlength in
		(fn |> file ~vfile) ~to_ ~offset length

	let access ~vpath ~vflags fn =
		let flags =
			List.map (fun v ->
				match decode_int v with
				| 0 -> `F_OK
				| 1 -> `R_OK
				| 2 -> `W_OK
				| 3 -> `X_OK
				| _ -> unexpected_value v "eval.luv.File.FileAccessFlag"
			) (decode_array vflags) in
		(fn |> path ~vpath) flags

	let utime ~vatime ~vmtime fn =
		let atime = num vatime
		and mtime = num vmtime in
		fn ~atime ~mtime

	let link ~vlink fn =
		let link = decode_native_string vlink in
		fn ~link

	let symlink ~vflags fn =
		let flags = decode_int_flags vflags in
		let dir = if List.mem 0 flags then Some true else None
		and junction = if List.mem 1 flags then Some true else None in
		fn ?dir ?junction

	let chown ~vuid ~vgid fn =
		let uid = decode_int vuid
		and gid = decode_int vgid in
		fn ~uid ~gid

	let readdir ~vdir ~vnumber_of_entries fn =
		let number_of_entries = decode_optional decode_int vnumber_of_entries in
		fn ?number_of_entries |> dir ~vdir
end

let file_fields = [
	"stdin", VHandle (HFile File.stdin);
	"stdout", VHandle (HFile File.stdout);
	"stderr", VHandle (HFile File.stderr);
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
	"open", vfun6 (fun vloop vpath vflags vmode vrequest vcallback ->
		let callback = encode_callback (fun f -> VHandle (HFile f)) vcallback in
		(File.open_ |> F.async ~vloop ~vrequest |> F.open_ ~vmode ~vpath ~vflags) callback;
		vnull
	);
	"close", vfun4 (fun vfile vloop vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.close |> F.async ~vloop ~vrequest |> F.file ~vfile) callback;
		vnull
	);
	"read", vfun6 (fun vfile vloop vfile_offset vbuffers vrequest vcallback ->
		let callback = encode_callback encode_size_t vcallback in
		(File.read |> F.async ~vloop ~vrequest |> F.data ~vfile_offset ~vfile ~vbuffers) callback;
		vnull
	);
	"write", vfun6 (fun vfile vloop vfile_offset vbuffers vrequest vcallback ->
		let callback = encode_callback encode_size_t vcallback in
		(File.write |> F.async ~vloop ~vrequest |> F.data ~vfile_offset ~vfile ~vbuffers) callback;
		vnull
	);
	"unlink", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.unlink |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"rename", vfun5 (fun vloop vpath vto vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.rename |> F.async ~vloop ~vrequest |> F.rename ~vpath ~vto) callback;
		vnull
	);
	"mkstemp", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback =
			encode_callback (fun (n,file) ->
				encode_obj [key_name,vnative_string n; key_file,VHandle (HFile file)]
			) vcallback
		in
		(File.mkstemp |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"mkdtemp", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback = encode_callback vnative_string vcallback in
		(File.mkdtemp |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"mkdir", vfun5 (fun vloop vpath vmode vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.mkdir |> F.async ~vloop ~vrequest |> F.mkdir ~vmode ~vpath) callback;
		vnull
	);
	"rmdir", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.rmdir |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"stat", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback = encode_callback encode_file_stat vcallback in
		(File.stat |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"lstat", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback = encode_callback encode_file_stat vcallback in
		(File.lstat |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"fstat", vfun4 (fun vfile vloop vrequest vcallback ->
		let callback = encode_callback encode_file_stat vcallback in
		(File.fstat |> F.async ~vloop ~vrequest |> F.file ~vfile) callback;
		vnull
	);
	"statFs", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback = encode_callback encode_file_statfs vcallback in
		(File.statfs |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"fsync", vfun4 (fun vfile vloop vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.fsync |> F.async ~vloop ~vrequest |> F.file ~vfile) callback;
		vnull
	);
	"fdataSync", vfun4 (fun vfile vloop vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.fdatasync |> F.async ~vloop ~vrequest |> F.file ~vfile) callback;
		vnull
	);
	"ftruncate", vfun5 (fun vfile vloop vlength vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.ftruncate |> F.async ~vloop ~vrequest |> F.ftruncate ~vfile ~vlength) callback;
		vnull
	);
	"copyFile", vfun6 (fun vloop vpath vto vflags vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.copyfile |> F.async ~vloop ~vrequest |> F.copyFile ~vflags ~vpath ~vto) callback;
		vnull
	);
	"sendFile", vfun7 (fun vfile vloop vto voffset vlength vrequest vcallback ->
		let callback = encode_callback encode_size_t vcallback in
		(File.sendfile |> F.async ~vloop ~vrequest |> F.sendFile ~vfile ~vto ~voffset ~vlength) callback;
		vnull
	);
	"access", vfun5 (fun vloop vpath vflags vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.access |> F.async ~vloop ~vrequest |> F.access ~vpath ~vflags) callback;
		vnull
	);
	"chmod", vfun5 (fun vloop vpath vmode vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.chmod |> F.async ~vloop ~vrequest |> F.path ~vpath |> F.mode ~vmode) callback;
		vnull
	);
	"fchmod", vfun5 (fun vfile vloop vmode vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.fchmod |> F.async ~vloop ~vrequest |> F.file ~vfile |> F.mode ~vmode) callback;
		vnull
	);
	"utime", vfun6 (fun vloop vpath vatime vmtime vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.utime |> F.async ~vloop ~vrequest |> F.path ~vpath |> F.utime ~vatime ~vmtime) callback;
		vnull
	);
	"lutime", vfun6 (fun vloop vpath vatime vmtime vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.lutime |> F.async ~vloop ~vrequest |> F.path ~vpath |> F.utime ~vatime ~vmtime) callback;
		vnull
	);
	"futime", vfun6 (fun vfile vloop vatime vmtime vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.futime |> F.async ~vloop ~vrequest |> F.file ~vfile |> F.utime ~vatime ~vmtime) callback;
		vnull
	);
	"link", vfun5 (fun vloop vpath vlink vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.link |> F.async ~vloop ~vrequest |> F.path ~vpath |> F.link ~vlink) callback;
		vnull
	);
	"symlink", vfun6 (fun vloop vpath vlink vflags vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.symlink |> F.async ~vloop ~vrequest |> F.symlink ~vflags |> F.path ~vpath |> F.link ~vlink) callback;
		vnull
	);
	"readLink", vfun4 (fun vloop vpath vrequest v4 ->
		let callback = encode_callback vnative_string v4 in
		(File.readlink |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"realPath", vfun4 (fun vloop vpath vrequest v4 ->
		let callback = encode_callback vnative_string v4 in
		(File.realpath |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"chown", vfun6 (fun vloop vpath vuid vgid vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.chown |> F.async ~vloop ~vrequest |> F.path ~vpath |> F.chown ~vuid ~vgid) callback;
		vnull
	);
	"lchown", vfun6 (fun vloop vpath vuid vgid vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.lchown |> F.async ~vloop ~vrequest |> F.path ~vpath |> F.chown ~vuid ~vgid) callback;
		vnull
	);
	"fchown", vfun6 (fun vfile vloop vuid vgid vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.fchown |> F.async ~vloop ~vrequest |> F.file ~vfile |> F.chown ~vuid ~vgid) callback;
		vnull
	);
	"toInt", vfun1 (fun v ->
		let file = decode_file v in
		vint (File.to_int file)
	);
]

let file_sync_fields = [
	"open", vfun3 (fun vpath vflags vmode ->
		File.Sync.open_ |> F.open_ ~vmode ~vpath ~vflags |> encode_result (fun f -> VHandle (HFile f))
	);
	"close", vfun1 (fun vfile ->
		File.Sync.close |> F.file ~vfile |> encode_unit_result
	);
	"read", vfun3 (fun vfile vfile_offset vbuffers ->
		File.Sync.read |> F.data ~vfile_offset ~vfile ~vbuffers |> encode_result encode_size_t
	);
	"write", vfun3 (fun vfile vfile_offset vbuffers ->
		File.Sync.write |> F.data ~vfile_offset ~vfile ~vbuffers |> encode_result encode_size_t
	);
	"unlink", vfun1 (fun vpath ->
		File.Sync.unlink |> F.path ~vpath |> encode_unit_result
	);
	"rename", vfun2 (fun vpath vto ->
		File.Sync.rename |> F.rename ~vpath ~vto |> encode_unit_result
	);
	"mkstemp", vfun1 (fun vpath ->
		let encode (n,file) =
			encode_obj [key_name,vnative_string n; key_file,VHandle (HFile file)]
		in
		File.Sync.mkstemp |> F.path ~vpath |> encode_result encode
	);
	"mkdtemp", vfun1 (fun vpath ->
		File.Sync.mkdtemp |> F.path ~vpath |> encode_result vnative_string
	);
	"mkdir", vfun2 (fun vpath vmode ->
		File.Sync.mkdir |> F.mkdir ~vmode ~vpath |> encode_unit_result
	);
	"rmdir", vfun1 (fun vpath ->
		File.Sync.rmdir |> F.path ~vpath |> encode_unit_result
	);
	"stat", vfun1 (fun vpath ->
		File.Sync.stat |> F.path ~vpath |> encode_result encode_file_stat
	);
	"lstat", vfun1 (fun vpath ->
		File.Sync.lstat |> F.path ~vpath |> encode_result encode_file_stat
	);
	"fstat", vfun1 (fun vfile ->
		File.Sync.fstat |> F.file ~vfile |> encode_result encode_file_stat
	);
	"statFs", vfun1 (fun vpath ->
		File.Sync.statfs |> F.path ~vpath |> encode_result encode_file_statfs
	);
	"fsync", vfun1 (fun vfile ->
		File.Sync.fsync |> F.file ~vfile |> encode_unit_result
	);
	"fdataSync", vfun1 (fun vfile ->
		File.Sync.fdatasync |> F.file ~vfile |> encode_unit_result
	);
	"ftruncate", vfun2 (fun vfile vlength ->
		File.Sync.ftruncate |> F.ftruncate ~vfile ~vlength |> encode_unit_result
	);
	"copyFile", vfun3 (fun vpath vto vflags ->
		File.Sync.copyfile |> F.copyFile ~vflags ~vpath ~vto |> encode_unit_result
	);
	"sendFile", vfun4 (fun vfile vto voffset vlength ->
		File.Sync.sendfile |> F.sendFile ~vfile ~vto ~voffset ~vlength |> encode_result encode_size_t
	);
	"access", vfun2 (fun vpath vflags ->
		File.Sync.access |> F.access ~vpath ~vflags |> encode_unit_result
	);
	"chmod", vfun2 (fun vpath vmode ->
		File.Sync.chmod |> F.path ~vpath |> F.mode ~vmode |> encode_unit_result
	);
	"fchmod", vfun2 (fun vfile vmode ->
		File.Sync.fchmod |> F.file ~vfile |> F.mode ~vmode |> encode_unit_result
	);
	"utime", vfun3 (fun vpath vatime vmtime ->
		File.Sync.utime |> F.path ~vpath |> F.utime ~vatime ~vmtime |> encode_unit_result
	);
	"lutime", vfun3 (fun vpath vatime vmtime ->
		File.Sync.lutime |> F.path ~vpath |> F.utime ~vatime ~vmtime |> encode_unit_result
	);
	"futime", vfun3 (fun vfile vatime vmtime ->
		File.Sync.futime |> F.file ~vfile |> F.utime ~vatime ~vmtime |> encode_unit_result
	);
	"link", vfun2 (fun vpath vlink ->
		File.Sync.link |> F.path ~vpath |> F.link ~vlink |> encode_unit_result
	);
	"symlink", vfun3 (fun vpath vlink vflags ->
		File.Sync.symlink |> F.symlink ~vflags |> F.path ~vpath |> F.link ~vlink |> encode_unit_result
	);
	"readLink", vfun1 (fun vpath ->
		File.Sync.readlink |> F.path ~vpath |> encode_result vnative_string
	);
	"realPath", vfun1 (fun vpath ->
		File.Sync.realpath |> F.path ~vpath |> encode_result vnative_string
	);
	"chown", vfun3 (fun vpath vuid vgid ->
		File.Sync.chown |> F.path ~vpath |> F.chown ~vuid ~vgid |> encode_unit_result
	);
	"lchown", vfun3 (fun vpath vuid vgid ->
		File.Sync.lchown |> F.path ~vpath |> F.chown ~vuid ~vgid |> encode_unit_result
	);
	"fchown", vfun3 (fun vfile vuid vgid ->
		File.Sync.fchown |> F.file ~vfile |> F.chown ~vuid ~vgid |> encode_unit_result
	);
]

let dir_fields = [
	"open", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback = encode_callback (fun dir -> VHandle (HDir dir)) vcallback in
		(File.opendir |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
	"close", vfun4 (fun vdir vloop vrequest vcallback ->
		let callback = encode_unit_callback vcallback in
		(File.closedir |> F.async ~vloop ~vrequest |> F.dir ~vdir) callback;
		vnull
	);
	"read", vfun5 (fun vdir vloop vnumber_of_entries vrequest vcallback ->
		let callback =
			encode_callback (fun a ->
				encode_array_a (Array.map encode_dirent a)
			) vcallback
		in
		(File.readdir |> F.async ~vloop ~vrequest |> F.readdir ~vnumber_of_entries ~vdir) callback;
		vnull
	);
	"scan", vfun4 (fun vloop vpath vrequest vcallback ->
		let callback = encode_callback encode_scandir vcallback in
		(File.scandir |> F.async ~vloop ~vrequest |> F.path ~vpath) callback;
		vnull
	);
]

let dir_sync_fields = [
	"open", vfun1 (fun vpath ->
		File.Sync.opendir |> F.path ~vpath |> encode_result (fun dir -> VHandle (HDir dir))
	);
	"close", vfun1 (fun vdir ->
		File.Sync.closedir |> F.dir ~vdir |> encode_unit_result
	);
	"read", vfun2 (fun vdir vnumber_of_entries ->
		let encode a =
			encode_array_a (Array.map encode_dirent a)
		in
		File.Sync.readdir |> F.readdir ~vnumber_of_entries ~vdir |> encode_result encode
	);
	"scan", vfun1 (fun vpath ->
		File.Sync.scandir |> F.path ~vpath |> encode_result encode_scandir
	);
]

let fs_event_fields = [
	"init", vfun1 (fun v ->
		let loop = decode_loop v in
		encode_result (fun e -> VHandle (HFsEvent e)) (FS_event.init ~loop ())
	);
	"start", vfun4 (fun v1 v2 v3 v4 ->
		let event = decode_fs_event v1
		and path = decode_native_string v2
		and callback =
			encode_callback (fun (file,events) ->
				let vevents =
					List.map (fun (e:FS_event.Event.t) ->
						match e with
						| `RENAME -> vint 0
						| `CHANGE -> vint 1
					) events
				in
				encode_obj [
					key_file,encode_nullable vnative_string file;
					key_events,encode_array vevents;
				]
			) v4
		in
		if v3 = VNull then
			FS_event.start event path callback
		else begin
			let flags = decode_int_flags v3 in
			let watch_entry = List.mem 0 flags
			and stat = List.mem 1 flags
			and recursive = List.mem 2 flags in
			FS_event.start ~watch_entry ~stat ~recursive event path callback
		end;
		vnull
	);
	"stop", vfun1 (fun v ->
		let event = decode_fs_event v in
		encode_unit_result (FS_event.stop event)
	);
]

let thread_pool_fields = [
	"createRequest", vfun0 (fun() ->
		VHandle (HThreadPoolRequest (Thread_pool.Request.make()))
	);
	"queueWork", vfun4 (fun v1 v2 v3 v4 ->
		let loop = decode_loop v1
		and request =
			decode_optional (function
				| VHandle (HThreadPoolRequest r) -> r
				| v -> unexpected_value v "eval.luv.ThreadPool.ThreadPoolRequest"
			) v2
		and work =
			let cb = prepare_callback v3 0 in
			(fun() -> EvalThread.run (get_ctx()) (fun() -> cb []))
		and callback = encode_unit_callback v4 in
		Thread_pool.queue_work ~loop ?request work callback;
		vnull
	);
	"setSize", vfun2 (fun v1 v2 ->
		let size = decode_int v1
		and if_not_already_set = decode_optional decode_bool v2 in
		Thread_pool.set_size ?if_not_already_set size;
		vnull
	);
]

let thread_fields = [
	"self", vfun0 (fun() ->
		VHandle (HThread (Thread.self()))
	);
	"create", vfun2 (fun v1 v2 ->
		let fn =
			let cb = prepare_callback v1 0 in
			(fun() -> EvalThread.run (get_ctx()) (fun() -> cb []))
		and stack_size = decode_optional decode_int v2 in
		encode_result (fun t -> VHandle (HThread t)) (Thread.create ?stack_size fn)
	);
	"join", vfun1 (fun v ->
		let thread =
			match v with
			| VHandle (HThread t) -> t
			| _ -> unexpected_value v "eval.luv.Thread"
		in
		encode_unit_result (Thread.join thread)
	);
]

let once_fields = [
	"init", vfun0 (fun() ->
		encode_result (fun o -> VHandle (HOnce o)) (Once.init())
	);
	"once", vfun2 (fun v1 v2 ->
		let once =
			match v1 with
			| VHandle (HOnce o) -> o
			| _ -> unexpected_value v1 "eval.luv.Once"
		and callback = prepare_callback v2 0 in
		Once.once once (fun() -> ignore(callback []));
		vnull
	);
]

let mutex_fields = [
	"init", vfun1 (fun v ->
		let recursive = decode_optional decode_bool v in
		encode_result (fun m -> VHandle (HMutex m)) (Mutex.init ?recursive ())
	);
	"destroy", vfun1 (fun v ->
		Mutex.destroy (decode_mutex v);
		vnull
	);
	"lock", vfun1 (fun v ->
		Mutex.lock (decode_mutex v);
		vnull
	);
	"tryLock", vfun1 (fun v ->
		encode_unit_result (Mutex.trylock (decode_mutex v))
	);
	"unlock", vfun1 (fun v ->
		Mutex.unlock (decode_mutex v);
		vnull
	);
]

let rwlock_fields = [
	"init", vfun0 (fun() ->
		encode_result (fun l -> VHandle (HRwLock l)) (Rwlock.init())
	);
	"destroy", vfun1 (fun v ->
		Rwlock.destroy (decode_rwlock v);
		vnull
	);
	"rdLock", vfun1 (fun v ->
		Rwlock.rdlock (decode_rwlock v);
		vnull
	);
	"rdTryLock", vfun1 (fun v ->
		encode_unit_result (Rwlock.tryrdlock (decode_rwlock v))
	);
	"rdUnlock", vfun1 (fun v ->
		Rwlock.rdunlock (decode_rwlock v);
		vnull
	);
	"wrLock", vfun1 (fun v ->
		Rwlock.wrlock (decode_rwlock v);
		vnull
	);
	"wrTryLock", vfun1 (fun v ->
		encode_unit_result (Rwlock.trywrlock (decode_rwlock v))
	);
	"wrUnlock", vfun1 (fun v ->
		Rwlock.wrunlock (decode_rwlock v);
		vnull
	);
]

let semaphore_fields = [
	"init", vfun1 (fun v ->
		encode_result (fun s -> VHandle (HSemaphore s)) (Semaphore.init (decode_int v))
	);
	"destroy", vfun1 (fun v ->
		Semaphore.destroy (decode_semaphore v);
		vnull
	);
	"post", vfun1 (fun v ->
		Semaphore.post (decode_semaphore v);
		vnull
	);
	"wait", vfun1 (fun v ->
		Semaphore.wait (decode_semaphore v);
		vnull
	);
	"tryWait", vfun1 (fun v ->
		encode_unit_result (Semaphore.trywait (decode_semaphore v))
	);
]

let condition_fields = [
	"init", vfun0 (fun() ->
		encode_result (fun s -> VHandle (HCondition s)) (Condition.init ())
	);
	"destroy", vfun1 (fun v ->
		Condition.destroy (decode_condition v);
		vnull
	);
	"signal", vfun1 (fun v ->
		Condition.signal (decode_condition v);
		vnull
	);
	"broadcast", vfun1 (fun v ->
		Condition.broadcast (decode_condition v);
		vnull
	);
	"wait", vfun2 (fun v1 v2 ->
		let condition = decode_condition v1
		and mutex = decode_mutex v2 in
		Condition.wait condition mutex;
		vnull
	);
	"timedWait", vfun3 (fun v1 v2 v3 ->
		let condition = decode_condition v1
		and mutex = decode_mutex v2
		and timeout = decode_int v3 in
		encode_unit_result (Condition.timedwait condition mutex timeout)
	);
]

let barrier_fields = [
	"init", vfun1 (fun v ->
		encode_result (fun b -> VHandle (HBarrier b)) (Barrier.init (decode_int v))
	);
	"destroy", vfun1 (fun v ->
		Barrier.destroy (decode_barrier v);
		vnull
	);
	"wait", vfun1 (fun v ->
		vbool (Barrier.wait (decode_barrier v))
	);
]

let env_fields = [
	"getEnv", vfun1 (fun v ->
		let name = decode_string v in
		encode_result vnative_string (Env.getenv name)
	);
	"setEnv", vfun2 (fun v1 v2 ->
		let name = decode_string v1
		and value = decode_native_string v2 in
		encode_unit_result (Env.setenv name ~value)
	);
	"unsetEnv", vfun1 (fun v ->
		let name = decode_string v in
		encode_unit_result (Env.unsetenv name)
	);
	"environ", vfun0 (fun() ->
		let encode env =
			let map =
				List.fold_left (fun map (name,value) ->
					StringHashtbl.add map (EvalString.create_unknown_vstring name) (vnative_string value);
					map
				) (StringHashtbl.create()) env
			in
			encode_string_map_direct map
		in
		encode_result encode (Env.environ())
	);
]

let time_fields = [
	"getTimeOfDay", vfun0 (fun() ->
		encode_result (fun (t:Time.t) ->
			encode_obj [key_sec,VInt64 t.sec; key_usec,vint32 t.usec]
		) (Time.gettimeofday())
	);
	"hrTime", vfun0 (fun() ->
		VUInt64 (Time.hrtime())
	);
	"sleep", vfun1 (fun v ->
		Time.sleep (decode_int v);
		vnull
	);
]

let path_fields = [
	"exePath", vfun0 (fun() ->
		encode_result vnative_string (Path.exepath())
	);
	"cwd", vfun0 (fun() ->
		encode_result vnative_string (Path.cwd())
	);
	"chdir", vfun1 (fun v ->
		encode_unit_result (Path.chdir (decode_native_string v))
	);
	"homedir", vfun0 (fun() ->
		encode_result vnative_string (Path.homedir())
	);
	"tmpdir", vfun0 (fun() ->
		encode_result vnative_string (Path.tmpdir())
	);
]

let random_fields = [
	"createRequest", vfun0 (fun() ->
		VHandle (HRandomRequest (Random.Request.make()))
	);
	"random", vfun4 (fun v1 v2 v3 v4 ->
		let loop = decode_loop v1
		and buffer = decode_buffer v2
		and request =
			decode_optional (function
				| VHandle (HRandomRequest r) -> r
				| v -> unexpected_value v "eval.luv.Random.RandomRequest"
			) v3
		and callback = encode_unit_callback v4 in
		Random.random ~loop ?request buffer callback;
		vnull
	);
]

let random_sync_fields = [
	"random", vfun1(fun v ->
		let buffer = decode_buffer v in
		encode_unit_result (Random.Sync.random buffer)
	);
]

let network_fields = [
	"interfaceAddresses", vfun0 (fun() ->
		encode_result (fun addresses ->
			encode_array (List.map (fun (a:Network.Interface_address.t) ->
				encode_obj [
					key_name, encode_string a.name;
					key_isInternal, vbool a.is_internal;
					key_physical, vnative_string a.physical;
					key_address, encode_sockaddr a.address;
					key_netmask, encode_sockaddr a.netmask;
				]
			) addresses)
		) (Network.interface_addresses())
	);
	"ifIndexToName", vfun1 (fun v ->
		let index = decode_int v in
		encode_result encode_string (Network.if_indextoname index)
	);
	"ifIndexToIid", vfun1 (fun v ->
		let index = decode_int v in
		encode_result encode_string (Network.if_indextoiid index)
	);
	"getHostName", vfun0 (fun() ->
		encode_result encode_string (Network.gethostname())
	);
]

let fs_poll_fields = [
	"init", vfun1 (fun v ->
		let loop = decode_loop v in
		encode_result (fun p -> VHandle (HFsPoll p)) (FS_poll.init ~loop ())
	);
	"start", vfun4 (fun v1 v2 v3 v4 ->
		let poll = decode_fs_poll v1
		and path = decode_native_string v2
		and interval = decode_optional decode_int v3
		and callback =
			encode_callback (fun (previous,current) ->
				encode_obj [
					key_previous,encode_file_stat previous;
					key_current,encode_file_stat current;
				]
			) v4
		in
		FS_poll.start ?interval poll path callback;
		vnull
	);
	"stop", vfun1 (fun v ->
		let poll = decode_fs_poll v in
		encode_unit_result (FS_poll.stop poll)
	);
]

let resource_fields = [
	"uptime", vfun0 (fun() ->
		encode_result vfloat (Resource.uptime());
	);
	"loadAvg", vfun0 (fun() ->
		let m1,m5,m15 = Resource.loadavg() in
		encode_array_a [|vfloat m1; vfloat m5; vfloat m15|];
	);
	"freeMemory", vfun0 (fun() ->
		encode_nullable (fun u -> VUInt64 u) (Resource.free_memory())
	);
	"totalMemory", vfun0 (fun() ->
		encode_nullable (fun u -> VUInt64 u) (Resource.total_memory())
	);
	"constrainedMemory", vfun0 (fun() ->
		encode_nullable (fun u -> VUInt64 u) (Resource.constrained_memory())
	);
	"getPriority", vfun1 (fun v ->
		let pid = decode_int v in
		encode_result vint (Resource.getpriority pid)
	);
	"setPriority", vfun2 (fun v1 v2 ->
		let pid = decode_int v1
		and priority = decode_int v2 in
		encode_unit_result (Resource.setpriority pid priority)
	);
	"residentSetMemory", vfun0 (fun() ->
		encode_result encode_size_t (Resource.resident_set_memory())
	);
	"getRUsage", vfun0 (fun() ->
		let encode_timeval (t:Resource.timeval) =
			encode_obj [
				key_sec, VInt64 (Signed.Long.to_int64 t.sec);
				key_usec, VInt64 (Signed.Long.to_int64 t.usec)
			]
		in
		let encode_rusage (r:Resource.rusage) =
			encode_obj_s [
				"utime", encode_timeval r.utime;
				"stime", encode_timeval r.stime;
				"maxrss", VUInt64 r.maxrss;
				"ixrss", VUInt64 r.ixrss;
				"idrss", VUInt64 r.idrss;
				"isrss", VUInt64 r.isrss;
				"minflt", VUInt64 r.minflt;
				"majflt", VUInt64 r.majflt;
				"nswap", VUInt64 r.nswap;
				"inblock", VUInt64 r.inblock;
				"oublock", VUInt64 r.oublock;
				"msgsnd", VUInt64 r.msgsnd;
				"msgrcv", VUInt64 r.msgrcv;
				"nsignals", VUInt64 r.nsignals;
				"nvcsw", VUInt64 r.nvcsw;
				"nivcsw", VUInt64 r.nivcsw;
			]
		in
		encode_result encode_rusage (Resource.getrusage())
	);
]

let system_info_fields = [
	"cpuInfo", vfun0 (fun() ->
		let encode_info (i:System_info.CPU_info.t) =
			encode_obj_s [
				"model", encode_string i.model;
				"speed", vint i.speed;
				"times", encode_obj_s [
					"user", VUInt64 i.times.user;
					"nice", VUInt64 i.times.nice;
					"sys", VUInt64 i.times.sys;
					"idle", VUInt64 i.times.idle;
					"irq", VUInt64 i.times.irq;
				]
			]
		in
		let encode l =
			encode_array (List.map encode_info l)
		in
		encode_result encode (System_info.cpu_info());
	);
	"uname", vfun0 (fun() ->
		encode_result (fun (u:System_info.Uname.t) ->
			encode_obj_s [
				"sysname", encode_string u.sysname;
				"release", encode_string u.release;
				"version", encode_string u.version;
				"machine", encode_string u.machine;
			]
		) (System_info.uname())
	);
]

let pid_fields = [
	"getPid", vfun0 (fun() ->
		vint (Pid.getpid())
	);
	"getPPid", vfun0 (fun() ->
		vint (Pid.getppid())
	);
]

let passwd_fields = [
	"getPasswd", vfun0 (fun() ->
		encode_result (fun (p:Passwd.t) ->
			encode_obj_s [
				"username",encode_string p.username;
				"uid",vint (Unsigned.ULong.to_int p.uid);
				"gid",vint (Unsigned.ULong.to_int p.gid);
				"shell",encode_nullable encode_string p.shell;
				"homedir",vnative_string p.homedir;
			]
		) (Passwd.get_passwd())
	);
]

let metrics_fields = [
	"idleTime", vfun1 (fun v ->
		let loop = decode_loop v in
		VUInt64 (Metrics.idle_time loop)
	);
]

let prepare_fields = [
	"init", vfun1 (fun v ->
		let loop = decode_loop v in
		encode_result (fun i -> VHandle (HPrepare i)) (Prepare.init ~loop ())
	);
	"start", vfun2 (fun v1 v2 ->
		let prepare = decode_prepare v1 in
		let cb = prepare_callback v2 0 in
		encode_unit_result (Prepare.start prepare (fun() -> ignore(cb [])));
	);
	"stop", vfun1 (fun v ->
		let prepare = decode_prepare v in
		encode_unit_result (Prepare.stop prepare)
	);
]

let check_fields = [
	"init", vfun1 (fun v ->
		let loop = decode_loop v in
		encode_result (fun i -> VHandle (HCheck i)) (Check.init ~loop ())
	);
	"start", vfun2 (fun v1 v2 ->
		let check = decode_check v1 in
		let cb = prepare_callback v2 0 in
		encode_unit_result (Check.start check (fun() -> ignore(cb [])));
	);
	"stop", vfun1 (fun v ->
		let check = decode_check v in
		encode_unit_result (Check.stop check)
	);
]

let version_fields = [
	"string", vfun0 (fun() -> encode_string (Version.string()));
	"major", vint (Version.major);
	"minor", vint (Version.minor);
	"patch", vint (Version.patch);
	"isRelease", vbool (Version.is_release);
	"suffix", encode_string (Version.suffix);
	"hex", vint (Version.hex);
]
