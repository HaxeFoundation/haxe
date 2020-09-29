open Unix

open Globals
open EvalValue
open EvalEncode
open EvalDecode
open EvalContext
open EvalExceptions
open EvalHash

let encode_error err =
	let index, args =
		match err with
		| E2BIG -> 0,[]
		| EACCES -> 1,[]
		| EAGAIN -> 2,[]
		| EBADF -> 3,[]
		| EBUSY -> 4,[]
		| ECHILD -> 5,[]
		| EDEADLK -> 6,[]
		| EDOM -> 7,[]
		| EEXIST -> 8,[]
		| EFAULT -> 9,[]
		| EFBIG -> 10,[]
		| EINTR -> 11,[]
		| EINVAL -> 12,[]
		| EIO -> 13,[]
		| EISDIR -> 14,[]
		| EMFILE -> 15,[]
		| EMLINK -> 16,[]
		| ENAMETOOLONG -> 17,[]
		| ENFILE -> 18,[]
		| ENODEV -> 19,[]
		| ENOENT -> 20,[]
		| ENOEXEC -> 21,[]
		| ENOLCK -> 22,[]
		| ENOMEM -> 23,[]
		| ENOSPC -> 24,[]
		| ENOSYS -> 25,[]
		| ENOTDIR -> 26,[]
		| ENOTEMPTY -> 27,[]
		| ENOTTY -> 28,[]
		| ENXIO -> 29,[]
		| EPERM -> 30,[]
		| EPIPE -> 31,[]
		| ERANGE -> 32,[]
		| EROFS -> 33,[]
		| ESPIPE -> 34,[]
		| ESRCH -> 35,[]
		| EXDEV -> 36,[]
		| EWOULDBLOCK -> 37,[]
		| EINPROGRESS -> 38,[]
		| EALREADY -> 39,[]
		| ENOTSOCK -> 40,[]
		| EDESTADDRREQ -> 41,[]
		| EMSGSIZE -> 42,[]
		| EPROTOTYPE -> 43,[]
		| ENOPROTOOPT -> 44,[]
		| EPROTONOSUPPORT -> 45,[]
		| ESOCKTNOSUPPORT -> 46,[]
		| EOPNOTSUPP -> 47,[]
		| EPFNOSUPPORT -> 48,[]
		| EAFNOSUPPORT -> 49,[]
		| EADDRINUSE -> 50,[]
		| EADDRNOTAVAIL -> 51,[]
		| ENETDOWN -> 52,[]
		| ENETUNREACH -> 53,[]
		| ENETRESET -> 54,[]
		| ECONNABORTED -> 55,[]
		| ECONNRESET -> 56,[]
		| ENOBUFS -> 57,[]
		| EISCONN -> 58,[]
		| ENOTCONN -> 59,[]
		| ESHUTDOWN -> 60,[]
		| ETOOMANYREFS -> 61,[]
		| ETIMEDOUT -> 62,[]
		| ECONNREFUSED -> 63,[]
		| EHOSTDOWN -> 64,[]
		| EHOSTUNREACH -> 65,[]
		| ELOOP -> 66,[]
		| EOVERFLOW -> 67,[]
		| EUNKNOWNERR code -> 68,[vint code]
	in
	encode_enum_value key_eval_PosixError index (Array.of_list args) None

(**
	Execute `fn` and re-throw any exceptions into Haxe code.
*)
let exec fn =
	try
		fn()
	with
	| Unix_error (err,_,_) ->
		throw (encode_error err) null_pos
	| e ->
		exc_string (Printexc.to_string e)

let is_link = vfun1 (fun v ->
	let path = decode_native_string v in
	let st = exec (fun () -> lstat path) in
	vbool (st.st_kind = S_LNK)
)

let is_file = vfun1 (fun v ->
	let path = decode_native_string v in
	let st = exec (fun () -> stat path) in
	vbool (st.st_kind = S_REG)
)

let is_directory = vfun1 (fun v ->
	let path = decode_native_string v in
	let st = exec (fun () -> stat path) in
	vbool (st.st_kind = S_DIR)
)

let encode_stat st =
	let kind =
		match st.st_kind with
		| S_REG -> 0
		| S_DIR -> 1
		| S_CHR -> 2
		| S_BLK -> 3
		| S_LNK -> 4
		| S_FIFO -> 5
		| S_SOCK -> 6
	in
	encode_obj_s [
		"st_dev",vint st.st_dev;
		"st_ino",vint st.st_ino;
		"st_kind",vint kind;
		"st_perm",vint st.st_perm;
		"st_nlink",vint st.st_nlink;
		"st_uid",vint st.st_uid;
		"st_gid",vint st.st_gid;
		"st_rdev",vint st.st_rdev;
		"st_size",vint st.st_size;
		"st_atime",vint (int_of_float st.st_atime);
		"st_mtime",vint (int_of_float st.st_mtime);
		"st_ctime",vint (int_of_float st.st_ctime);
	]

let get_stat = vfun1 (fun v ->
	let path = decode_native_string v in
	encode_stat (exec (fun () -> stat path))
)

let get_lstat = vfun1 (fun v ->
	let path = decode_native_string v in
	encode_stat (exec (fun () -> lstat path))
)

let set_utimes = vfun3 (fun v_path v_access v_modification ->
	let path = decode_native_string v_path
	and access_time = match v_access with VInt32 i32 -> Int32.to_float i32 | v -> decode_float v
	and modification_time = match v_modification with VInt32 i32 -> Int32.to_float i32 | v -> decode_float v in
	exec (fun() -> utimes path access_time modification_time);
	vnull
)

let get_real_path = vfun1 (fun v ->
	let path = decode_native_string v in
	vnative_string (exec (fun () ->
		if Globals.is_windows then
			Extc.get_real_path path
		else
			Extc.get_full_path path
	))
)

let mkdir = vfun3 (fun v_path v_permissions v_recursive ->
	let path = decode_native_string v_path
	and permissions = decode_int v_permissions
	and recursive = decode_bool v_recursive in
	let rec create path =
		try
			mkdir path permissions
		with Unix_error (ENOENT, _, _) when recursive ->
			create (Filename.dirname path);
			mkdir path permissions
	in
	exec (fun () -> create path);
	vnull
)

let open_file = vfun3 (fun v_path v_flags v_permissions ->
	let decode_flag v =
		match decode_int v with
		| 0 -> O_RDONLY
		| 1 -> O_WRONLY
		| 2 -> O_RDWR
		| 3 -> O_NONBLOCK
		| 4 -> O_APPEND
		| 5 -> O_CREAT
		| 6 -> O_TRUNC
		| 7 -> O_EXCL
		| 8 -> O_NOCTTY
		| 9 -> O_DSYNC
		| 10 -> O_SYNC
		| 11 -> O_RSYNC
		| 12 -> O_SHARE_DELETE
		| 13 -> O_CLOEXEC
		| 14 -> O_KEEPEXEC
		| i -> exc_string ("Unknown OpenFlag value: " ^ (string_of_int i))
	in
	let path = decode_native_string v_path
	and flags = List.map decode_flag (decode_array v_flags)
	and permissions = decode_int v_permissions in
	vfile_descriptor (exec (fun () -> openfile path flags permissions))
)

let close_file = vfun1 (fun v ->
	let fd = decode_file_descriptor v in
	exec (fun () -> close fd);
	vnull
)

let read_file = vfun4 (fun v_file v_buffer v_pos v_length ->
	let fd = decode_file_descriptor v_file
	and b = decode_bytes v_buffer
	and pos = decode_int v_pos
	and length = decode_int v_length in
	vint (exec (fun () -> read fd b pos length))
)

let write_file = vfun4 (fun v_file v_buffer v_pos v_length ->
	let fd = decode_file_descriptor v_file
	and b = decode_bytes v_buffer
	and pos = decode_int v_pos
	and length = decode_int v_length in
	vint (exec (fun () -> write fd b pos length))
)