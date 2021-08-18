package hl.uv;

/**
	TODO:
	Find a way to assign native error numbers to these constants.
**/
enum abstract UVError(Int) {
	/** No error. */
	var UV_NOERR = 0;

	var UV_E2BIG = 1;
	var UV_EACCES = 2;
	var UV_EADDRINUSE = 3;
	var UV_EADDRNOTAVAIL = 4;
	var UV_EAFNOSUPPORT = 5;
	var UV_EAGAIN = 6;
	var UV_EAI_ADDRFAMILY = 7;
	var UV_EAI_AGAIN = 8;
	var UV_EAI_BADFLAGS = 9;
	var UV_EAI_BADHINTS = 10;
	var UV_EAI_CANCELED = 11;
	var UV_EAI_FAIL = 12;
	var UV_EAI_FAMILY = 13;
	var UV_EAI_MEMORY = 14;
	var UV_EAI_NODATA = 15;
	var UV_EAI_NONAME = 16;
	var UV_EAI_OVERFLOW = 17;
	var UV_EAI_PROTOCOL = 18;
	var UV_EAI_SERVICE = 19;
	var UV_EAI_SOCKTYPE = 20;
	var UV_EALREADY = 21;
	var UV_EBADF = 22;
	var UV_EBUSY = 23;
	var UV_ECANCELED = 24;
	var UV_ECHARSET = 25;
	var UV_ECONNABORTED = 26;
	var UV_ECONNREFUSED = 27;
	var UV_ECONNRESET = 28;
	var UV_EDESTADDRREQ = 29;
	var UV_EEXIST = 30;
	var UV_EFAULT = 31;
	var UV_EFBIG = 32;
	var UV_EHOSTUNREACH = 33;
	var UV_EINTR = 34;
	var UV_EINVAL = 35;
	var UV_EIO = 36;
	var UV_EISCONN = 37;
	var UV_EISDIR = 38;
	var UV_ELOOP = 39;
	var UV_EMFILE = 40;
	var UV_EMSGSIZE = 41;
	var UV_ENAMETOOLONG = 42;
	var UV_ENETDOWN = 43;
	var UV_ENETUNREACH = 44;
	var UV_ENFILE = 45;
	var UV_ENOBUFS = 46;
	var UV_ENODEV = 47;
	var UV_ENOENT = 48;
	var UV_ENOMEM = 49;
	var UV_ENONET = 50;
	var UV_ENOPROTOOPT = 51;
	var UV_ENOSPC = 52;
	var UV_ENOSYS = 53;
	var UV_ENOTCONN = 54;
	var UV_ENOTDIR = 55;
	var UV_ENOTEMPTY = 56;
	var UV_ENOTSOCK = 57;
	var UV_ENOTSUP = 58;
	var UV_EOVERFLOW = 59;
	var UV_EPERM = 60;
	var UV_EPIPE = 61;
	var UV_EPROTO = 62;
	var UV_EPROTONOSUPPORT = 63;
	var UV_EPROTOTYPE = 64;
	var UV_ERANGE = 65;
	var UV_EROFS = 66;
	var UV_ESHUTDOWN = 67;
	var UV_ESPIPE = 68;
	var UV_ESRCH = 69;
	var UV_ETIMEDOUT = 70;
	var UV_ETXTBSY = 71;
	var UV_EXDEV = 72;
	var UV_UNKNOWN = 73;
	var UV_EOF = 74;
	var UV_ENXIO = 75;
	var UV_EMLINK = 76;
	var UV_ENOTTY = 77;
	var UV_EFTYPE = 78;
	var UV_EILSEQ = 79;
	var UV_ESOCKTNOSUPPORT = 80;

	public function toString():String {
		return 'UVError ' + inline name() + ': ' + inline description();
	}

	public function name():String {
		return @:privateAccess String.fromUTF8(this.translate_to_uv_error().err_name());
	}

	public function description():String {
		return @:privateAccess String.fromUTF8(this.translate_to_uv_error().strerror());
	}
}