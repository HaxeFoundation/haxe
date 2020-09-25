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

let is_link = vfun1 (fun v ->
	let path = decode_native_string v in
	let info =
		try
			lstat path
		with Unix_error (err,_,_) ->
			throw (encode_error err) null_pos
	in
	vbool (info.st_kind = S_LNK)
)