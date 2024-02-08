package eval.luv;

/**
	Error handling.

	@see https://aantron.github.io/luv/luv/Luv/Error
**/
enum abstract UVError(Int) {
	/** argument list too long */
	var UV_E2BIG = 0;
	/** permission denied */
	var UV_EACCES = 1;
	/** address already in use */
	var UV_EADDRINUSE = 2;
	/** address not available */
	var UV_EADDRNOTAVAIL = 3;
	/** address family not supported */
	var UV_EAFNOSUPPORT = 4;
	/** resource temporarily unavailable */
	var UV_EAGAIN = 5;
	/** address family not supported */
	var UV_EAI_ADDRFAMILY = 6;
	/** temporary failure */
	var UV_EAI_AGAIN = 7;
	/** bad ai_flags value */
	var UV_EAI_BADFLAGS = 8;
	/** invalid value for hints */
	var UV_EAI_BADHINTS = 9;
	/** request canceled */
	var UV_EAI_CANCELED = 10;
	/** permanent failure */
	var UV_EAI_FAIL = 11;
	/** ai_family not supported */
	var UV_EAI_FAMILY = 12;
	/** out of memory */
	var UV_EAI_MEMORY = 13;
	/** no address */
	var UV_EAI_NODATA = 14;
	/** unknown node or service */
	var UV_EAI_NONAME = 15;
	/** argument buffer overflow */
	var UV_EAI_OVERFLOW = 16;
	/** resolved protocol is unknown */
	var UV_EAI_PROTOCOL = 17;
	/** service not available for socket type */
	var UV_EAI_SERVICE = 18;
	/** socket type not supported */
	var UV_EAI_SOCKTYPE = 19;
	/** connection already in progress */
	var UV_EALREADY = 20;
	/** bad file descriptor */
	var UV_EBADF = 21;
	/** resource busy or locked */
	var UV_EBUSY = 22;
	/** operation canceled */
	var UV_ECANCELED = 23;
	/** invalid Unicode character */
	// var UV_ECHARSET = 24; // not defined in Luv ocaml library
	/** software caused connection abort */
	var UV_ECONNABORTED = 25;
	/** connection refused */
	var UV_ECONNREFUSED = 26;
	/** connection reset by peer */
	var UV_ECONNRESET = 27;
	/** destination address required */
	var UV_EDESTADDRREQ = 28;
	/** file already exists */
	var UV_EEXIST = 29;
	/** bad address in system call argument */
	var UV_EFAULT = 30;
	/** file too large */
	var UV_EFBIG = 31;
	/** host is unreachable */
	var UV_EHOSTUNREACH = 32;
	/** interrupted system call */
	var UV_EINTR = 33;
	/** invalid argument */
	var UV_EINVAL = 34;
	/** i/o error */
	var UV_EIO = 35;
	/** socket is already connected */
	var UV_EISCONN = 36;
	/** illegal operation on a directory */
	var UV_EISDIR = 37;
	/** too many symbolic links encountered */
	var UV_ELOOP = 38;
	/** too many open files */
	var UV_EMFILE = 39;
	/** message too long */
	var UV_EMSGSIZE = 40;
	/** name too long */
	var UV_ENAMETOOLONG = 41;
	/** network is down */
	var UV_ENETDOWN = 42;
	/** network is unreachable */
	var UV_ENETUNREACH = 43;
	/** file table overflow */
	var UV_ENFILE = 44;
	/** no buffer space available */
	var UV_ENOBUFS = 45;
	/** no such device */
	var UV_ENODEV = 46;
	/** no such file or directory */
	var UV_ENOENT = 47;
	/** not enough memory */
	var UV_ENOMEM = 48;
	/** machine is not on the network */
	var UV_ENONET = 49;
	/** protocol not available */
	var UV_ENOPROTOOPT = 50;
	/** no space left on device */
	var UV_ENOSPC = 51;
	/** function not implemented */
	var UV_ENOSYS = 52;
	/** socket is not connected */
	var UV_ENOTCONN = 53;
	/** not a directory */
	var UV_ENOTDIR = 54;
	/** directory not empty */
	var UV_ENOTEMPTY = 55;
	/** socket operation on non-socket */
	var UV_ENOTSOCK = 56;
	/** operation not supported on socket */
	var UV_ENOTSUP = 57;
	/** operation not permitted */
	var UV_EPERM = 58;
	/** broken pipe */
	var UV_EPIPE = 59;
	/** protocol error */
	var UV_EPROTO = 60;
	/** protocol not supported */
	var UV_EPROTONOSUPPORT = 61;
	/** protocol wrong type for socket */
	var UV_EPROTOTYPE = 62;
	/** result too large */
	var UV_ERANGE = 63;
	/** read-only file system */
	var UV_EROFS = 64;
	/** cannot send after transport endpoint shutdown */
	var UV_ESHUTDOWN = 65;
	/** invalid seek */
	var UV_ESPIPE = 66;
	/** no such process */
	var UV_ESRCH = 67;
	/** connection timed out */
	var UV_ETIMEDOUT = 68;
	/** text file is busy */
	var UV_ETXTBSY = 69;
	/** cross-device link not permitted */
	var UV_EXDEV = 70;
	/** unknown error */
	var UV_UNKNOWN = 71;
	/** end of file */
	var UV_EOF = 72;
	/** no such device or address */
	var UV_ENXIO = 73;
	/** too many links */
	var UV_EMLINK = 74;
	/** inappropriate ioctl for device */
	var UV_ENOTTY = 75;
	/** inappropriate file type or format */
	var UV_EFTYPE = 76;
	/** illegal byte sequence */
	var UV_EILSEQ = 77;
	/** value too large for defined data type **/
	var UV_EOVERFLOW = 78;
	/** socket type not supported**/
	var UV_ESOCKTNOSUPPORT = 79;

	/**
		Converts a system error code to a libuv error.
	**/
	extern static public function translateSysError(code:Int):UVError;

	/**
		Setup a callback for unhandled exceptions.

		@see https://aantron.github.io/luv/luv/Luv/Error#val-set_on_unhandled_exception
	**/
	extern static public function setOnUnhandledException(callback:(e:haxe.Exception)->Void):Void;

	/**
		Returns the name of the given error.
	**/
	extern public function errName():String;

	/**
		Returns the error message corresponding to the given error.
	**/
	extern public function toString():String;
}