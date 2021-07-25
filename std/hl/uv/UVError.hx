package hl.uv;

/**
	TODO:
	Find a way to assign native error numbers to these constants.
**/
enum abstract UVError(Int) {
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
	var UV_EAI_FAIL = 11;
	var UV_EAI_FAMILY = 12;
	var UV_EAI_MEMORY = 13;
	var UV_EAI_NODATA = 14;
	var UV_EAI_NONAME = 15;
	var UV_EAI_OVERFLOW = 16;
	var UV_EAI_PROTOCOL = 17;
	var UV_EAI_SERVICE = 18;
	var UV_EAI_SOCKTYPE = 19;
	var UV_EALREADY = 20;
	var UV_EBADF = 21;
	var UV_EBUSY = 22;
	var UV_ECANCELED = 23;
	var UV_ECHARSET = 24;
	var UV_ECONNABORTED = 25;
	var UV_ECONNREFUSED = 26;
	var UV_ECONNRESET = 27;
	var UV_EDESTADDRREQ = 28;
	var UV_EEXIST = 29;
	var UV_EFAULT = 30;
	var UV_EFBIG = 31;
	var UV_EHOSTUNREACH = 32;
	var UV_EINTR = 33;
	var UV_EINVAL = 34;
	var UV_EIO = 35;
	var UV_EISCONN = 36;
	var UV_EISDIR = 37;
	var UV_ELOOP = 38;
	var UV_EMFILE = 39;
	var UV_EMSGSIZE = 40;
	var UV_ENAMETOOLONG = 41;
	var UV_ENETDOWN = 42;
	var UV_ENETUNREACH = 43;
	var UV_ENFILE = 44;
	var UV_ENOBUFS = 45;
	var UV_ENODEV = 46;
	var UV_ENOENT = 47;
	var UV_ENOMEM = 48;
	var UV_ENONET = 49;
	var UV_ENOPROTOOPT = 50;
	var UV_ENOSPC = 51;
	var UV_ENOSYS = 52;
	var UV_ENOTCONN = 53;
	var UV_ENOTDIR = 54;
	var UV_ENOTEMPTY = 55;
	var UV_ENOTSOCK = 56;
	var UV_ENOTSUP = 57;
	var UV_EPERM = 58;
	var UV_EPIPE = 59;
	var UV_EPROTO = 60;
	var UV_EPROTONOSUPPORT = 61;
	var UV_EPROTOTYPE = 62;
	var UV_ERANGE = 63;
	var UV_EROFS = 64;
	var UV_ESHUTDOWN = 65;
	var UV_ESPIPE = 66;
	var UV_ESRCH = 67;
	var UV_ETIMEDOUT = 68;
	var UV_ETXTBSY = 69;
	var UV_EXDEV = 70;
	var UV_UNKNOWN = 71;
	var UV_EOF = 72;
	var UV_ENXIO = 73;
	var UV_EMLINK = 74;
	var UV_EHOSTDOWN = 75;
	var UV_EREMOTEIO = 76;
	var UV_ENOTTY = 77;

	public function toString():String {
		// return 'UVError.toString not implemented';
		return 'UVError #$this';
	}
}