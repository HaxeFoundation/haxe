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
	var UV_EPERM = 59;
	var UV_EPIPE = 60;
	var UV_EPROTO = 61;
	var UV_EPROTONOSUPPORT = 62;
	var UV_EPROTOTYPE = 63;
	var UV_ERANGE = 64;
	var UV_EROFS = 65;
	var UV_ESHUTDOWN = 66;
	var UV_ESPIPE = 67;
	var UV_ESRCH = 68;
	var UV_ETIMEDOUT = 69;
	var UV_ETXTBSY = 70;
	var UV_EXDEV = 71;
	var UV_UNKNOWN = 72;
	var UV_EOF = 73;
	var UV_ENXIO = 74;
	var UV_EMLINK = 75;
	var UV_EHOSTDOWN = 76;
	var UV_EREMOTEIO = 77;
	var UV_ENOTTY = 78;

	public function toString():String {
		return 'UVError ' + name();
	}

	function name():String {
		return switch (cast this:UVError) {
			case UV_NOERR: "NOERR";
			case UV_E2BIG: "E2BIG";
			case UV_EACCES: "EACCES";
			case UV_EADDRINUSE: "EADDRINUSE";
			case UV_EADDRNOTAVAIL: "EADDRNOTAVAIL";
			case UV_EAFNOSUPPORT: "EAFNOSUPPORT";
			case UV_EAGAIN: "EAGAIN";
			case UV_EAI_ADDRFAMILY: "EAI_ADDRFAMILY";
			case UV_EAI_AGAIN: "EAI_AGAIN";
			case UV_EAI_BADFLAGS: "EAI_BADFLAGS";
			case UV_EAI_BADHINTS: "EAI_BADHINTS";
			case UV_EAI_CANCELED: "EAI_CANCELED";
			case UV_EAI_FAIL: "EAI_FAIL";
			case UV_EAI_FAMILY: "EAI_FAMILY";
			case UV_EAI_MEMORY: "EAI_MEMORY";
			case UV_EAI_NODATA: "EAI_NODATA";
			case UV_EAI_NONAME: "EAI_NONAME";
			case UV_EAI_OVERFLOW: "EAI_OVERFLOW";
			case UV_EAI_PROTOCOL: "EAI_PROTOCOL";
			case UV_EAI_SERVICE: "EAI_SERVICE";
			case UV_EAI_SOCKTYPE: "EAI_SOCKTYPE";
			case UV_EALREADY: "EALREADY";
			case UV_EBADF: "EBADF";
			case UV_EBUSY: "EBUSY";
			case UV_ECANCELED: "ECANCELED";
			case UV_ECHARSET: "ECHARSET";
			case UV_ECONNABORTED: "ECONNABORTED";
			case UV_ECONNREFUSED: "ECONNREFUSED";
			case UV_ECONNRESET: "ECONNRESET";
			case UV_EDESTADDRREQ: "EDESTADDRREQ";
			case UV_EEXIST: "EEXIST";
			case UV_EFAULT: "EFAULT";
			case UV_EFBIG: "EFBIG";
			case UV_EHOSTUNREACH: "EHOSTUNREACH";
			case UV_EINTR: "EINTR";
			case UV_EINVAL: "EINVAL";
			case UV_EIO: "EIO";
			case UV_EISCONN: "EISCONN";
			case UV_EISDIR: "EISDIR";
			case UV_ELOOP: "ELOOP";
			case UV_EMFILE: "EMFILE";
			case UV_EMSGSIZE: "EMSGSIZE";
			case UV_ENAMETOOLONG: "ENAMETOOLONG";
			case UV_ENETDOWN: "ENETDOWN";
			case UV_ENETUNREACH: "ENETUNREACH";
			case UV_ENFILE: "ENFILE";
			case UV_ENOBUFS: "ENOBUFS";
			case UV_ENODEV: "ENODEV";
			case UV_ENOENT: "ENOENT";
			case UV_ENOMEM: "ENOMEM";
			case UV_ENONET: "ENONET";
			case UV_ENOPROTOOPT: "ENOPROTOOPT";
			case UV_ENOSPC: "ENOSPC";
			case UV_ENOSYS: "ENOSYS";
			case UV_ENOTCONN: "ENOTCONN";
			case UV_ENOTDIR: "ENOTDIR";
			case UV_ENOTEMPTY: "ENOTEMPTY";
			case UV_ENOTSOCK: "ENOTSOCK";
			case UV_ENOTSUP: "ENOTSUP";
			case UV_EPERM: "EPERM";
			case UV_EPIPE: "EPIPE";
			case UV_EPROTO: "EPROTO";
			case UV_EPROTONOSUPPORT: "EPROTONOSUPPORT";
			case UV_EPROTOTYPE: "EPROTOTYPE";
			case UV_ERANGE: "ERANGE";
			case UV_EROFS: "EROFS";
			case UV_ESHUTDOWN: "ESHUTDOWN";
			case UV_ESPIPE: "ESPIPE";
			case UV_ESRCH: "ESRCH";
			case UV_ETIMEDOUT: "ETIMEDOUT";
			case UV_ETXTBSY: "ETXTBSY";
			case UV_EXDEV: "EXDEV";
			case UV_UNKNOWN: "UNKNOWN";
			case UV_EOF: "EOF";
			case UV_ENXIO: "ENXIO";
			case UV_EMLINK: "EMLINK";
			case UV_EHOSTDOWN: "EHOSTDOWN";
			case UV_EREMOTEIO: "EREMOTEIO";
			case UV_ENOTTY: "ENOTTY";
			case _: '#$this';
		}
	}
}