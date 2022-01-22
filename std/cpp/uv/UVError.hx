package cpp.uv;

using cpp.uv.UV;

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
		return UVErrorTools.name(cast this);
	}

	public function description():String {
		return UVErrorTools.description(cast this);
	}

	static public function ofNative(e:Int):UVError {
		return UVErrorTools.ofNative(e);
	}

	public function toNative():Int {
		return UVErrorTools.toNative(cast this);
	}
}

@:cppInclude('uv.h')
private class UVErrorTools {
	static public function name(e:UVError):String {
		return switch toNative(e) {
			case 0: 'UV_NOERR';
			case e: UV.err_name(e).toString();
		}
	}

	static public function description(e:UVError):String {
		return switch toNative(e) {
			case 0: 'no error';
			case e: UV.strerror(e).toString();
		}
	}

	static public function ofNative(e:Int):UVError {
		untyped __cpp__('
			switch({0}) {
				case 0: return 0;
				case UV_E2BIG: return 1;
				case UV_EACCES: return 2;
				case UV_EADDRINUSE: return 3;
				case UV_EADDRNOTAVAIL: return 4;
				case UV_EAFNOSUPPORT: return 5;
				case UV_EAGAIN: return 6;
				case UV_EAI_ADDRFAMILY: return 7;
				case UV_EAI_AGAIN: return 8;
				case UV_EAI_BADFLAGS: return 9;
				case UV_EAI_BADHINTS: return 10;
				case UV_EAI_CANCELED: return 11;
				case UV_EAI_FAIL: return 12;
				case UV_EAI_FAMILY: return 13;
				case UV_EAI_MEMORY: return 14;
				case UV_EAI_NODATA: return 15;
				case UV_EAI_NONAME: return 16;
				case UV_EAI_OVERFLOW: return 17;
				case UV_EAI_PROTOCOL: return 18;
				case UV_EAI_SERVICE: return 19;
				case UV_EAI_SOCKTYPE: return 20;
				case UV_EALREADY: return 21;
				case UV_EBADF: return 22;
				case UV_EBUSY: return 23;
				case UV_ECANCELED: return 24;
				case UV_ECHARSET: return 25;
				case UV_ECONNABORTED: return 26;
				case UV_ECONNREFUSED: return 27;
				case UV_ECONNRESET: return 28;
				case UV_EDESTADDRREQ: return 29;
				case UV_EEXIST: return 30;
				case UV_EFAULT: return 31;
				case UV_EFBIG: return 32;
				case UV_EHOSTUNREACH: return 33;
				case UV_EINTR: return 34;
				case UV_EINVAL: return 35;
				case UV_EIO: return 36;
				case UV_EISCONN: return 37;
				case UV_EISDIR: return 38;
				case UV_ELOOP: return 39;
				case UV_EMFILE: return 40;
				case UV_EMSGSIZE: return 41;
				case UV_ENAMETOOLONG: return 42;
				case UV_ENETDOWN: return 43;
				case UV_ENETUNREACH: return 44;
				case UV_ENFILE: return 45;
				case UV_ENOBUFS: return 46;
				case UV_ENODEV: return 47;
				case UV_ENOENT: return 48;
				case UV_ENOMEM: return 49;
				case UV_ENONET: return 50;
				case UV_ENOPROTOOPT: return 51;
				case UV_ENOSPC: return 52;
				case UV_ENOSYS: return 53;
				case UV_ENOTCONN: return 54;
				case UV_ENOTDIR: return 55;
				case UV_ENOTEMPTY: return 56;
				case UV_ENOTSOCK: return 57;
				case UV_ENOTSUP: return 58;
				case UV_EOVERFLOW: return 59;
				case UV_EPERM: return 60;
				case UV_EPIPE: return 61;
				case UV_EPROTO: return 62;
				case UV_EPROTONOSUPPORT: return 63;
				case UV_EPROTOTYPE: return 64;
				case UV_ERANGE: return 65;
				case UV_EROFS: return 66;
				case UV_ESHUTDOWN: return 67;
				case UV_ESPIPE: return 68;
				case UV_ESRCH: return 69;
				case UV_ETIMEDOUT: return 70;
				case UV_ETXTBSY: return 71;
				case UV_EXDEV: return 72;
				case UV_UNKNOWN: return 73;
				case UV_EOF: return 74;
				case UV_ENXIO: return 75;
				case UV_EMLINK: return 76;
				case UV_ENOTTY: return 77;
				case UV_EFTYPE: return 78;
				case UV_EILSEQ: return 79;
				case UV_ESOCKTNOSUPPORT: return 80;
			}
		', e);
		return UV_UNKNOWN;
	}

	static public function toNative(e:UVError):Int {
		return switch e {
			case UV_NOERR: 0;
			case UV_E2BIG: untyped __cpp__('UV_E2BIG');
			case UV_EACCES: untyped __cpp__('UV_EACCES');
			case UV_EADDRINUSE: untyped __cpp__('UV_EADDRINUSE');
			case UV_EADDRNOTAVAIL: untyped __cpp__('UV_EADDRNOTAVAIL');
			case UV_EAFNOSUPPORT: untyped __cpp__('UV_EAFNOSUPPORT');
			case UV_EAGAIN: untyped __cpp__('UV_EAGAIN');
			case UV_EAI_ADDRFAMILY: untyped __cpp__('UV_EAI_ADDRFAMILY');
			case UV_EAI_AGAIN: untyped __cpp__('UV_EAI_AGAIN');
			case UV_EAI_BADFLAGS: untyped __cpp__('UV_EAI_BADFLAGS');
			case UV_EAI_BADHINTS: untyped __cpp__('UV_EAI_BADHINTS');
			case UV_EAI_CANCELED: untyped __cpp__('UV_EAI_CANCELED');
			case UV_EAI_FAIL: untyped __cpp__('UV_EAI_FAIL');
			case UV_EAI_FAMILY: untyped __cpp__('UV_EAI_FAMILY');
			case UV_EAI_MEMORY: untyped __cpp__('UV_EAI_MEMORY');
			case UV_EAI_NODATA: untyped __cpp__('UV_EAI_NODATA');
			case UV_EAI_NONAME: untyped __cpp__('UV_EAI_NONAME');
			case UV_EAI_OVERFLOW: untyped __cpp__('UV_EAI_OVERFLOW');
			case UV_EAI_PROTOCOL: untyped __cpp__('UV_EAI_PROTOCOL');
			case UV_EAI_SERVICE: untyped __cpp__('UV_EAI_SERVICE');
			case UV_EAI_SOCKTYPE: untyped __cpp__('UV_EAI_SOCKTYPE');
			case UV_EALREADY: untyped __cpp__('UV_EALREADY');
			case UV_EBADF: untyped __cpp__('UV_EBADF');
			case UV_EBUSY: untyped __cpp__('UV_EBUSY');
			case UV_ECANCELED: untyped __cpp__('UV_ECANCELED');
			case UV_ECHARSET: untyped __cpp__('UV_ECHARSET');
			case UV_ECONNABORTED: untyped __cpp__('UV_ECONNABORTED');
			case UV_ECONNREFUSED: untyped __cpp__('UV_ECONNREFUSED');
			case UV_ECONNRESET: untyped __cpp__('UV_ECONNRESET');
			case UV_EDESTADDRREQ: untyped __cpp__('UV_EDESTADDRREQ');
			case UV_EEXIST: untyped __cpp__('UV_EEXIST');
			case UV_EFAULT: untyped __cpp__('UV_EFAULT');
			case UV_EFBIG: untyped __cpp__('UV_EFBIG');
			case UV_EHOSTUNREACH: untyped __cpp__('UV_EHOSTUNREACH');
			case UV_EINTR: untyped __cpp__('UV_EINTR');
			case UV_EINVAL: untyped __cpp__('UV_EINVAL');
			case UV_EIO: untyped __cpp__('UV_EIO');
			case UV_EISCONN: untyped __cpp__('UV_EISCONN');
			case UV_EISDIR: untyped __cpp__('UV_EISDIR');
			case UV_ELOOP: untyped __cpp__('UV_ELOOP');
			case UV_EMFILE: untyped __cpp__('UV_EMFILE');
			case UV_EMSGSIZE: untyped __cpp__('UV_EMSGSIZE');
			case UV_ENAMETOOLONG: untyped __cpp__('UV_ENAMETOOLONG');
			case UV_ENETDOWN: untyped __cpp__('UV_ENETDOWN');
			case UV_ENETUNREACH: untyped __cpp__('UV_ENETUNREACH');
			case UV_ENFILE: untyped __cpp__('UV_ENFILE');
			case UV_ENOBUFS: untyped __cpp__('UV_ENOBUFS');
			case UV_ENODEV: untyped __cpp__('UV_ENODEV');
			case UV_ENOENT: untyped __cpp__('UV_ENOENT');
			case UV_ENOMEM: untyped __cpp__('UV_ENOMEM');
			case UV_ENONET: untyped __cpp__('UV_ENONET');
			case UV_ENOPROTOOPT: untyped __cpp__('UV_ENOPROTOOPT');
			case UV_ENOSPC: untyped __cpp__('UV_ENOSPC');
			case UV_ENOSYS: untyped __cpp__('UV_ENOSYS');
			case UV_ENOTCONN: untyped __cpp__('UV_ENOTCONN');
			case UV_ENOTDIR: untyped __cpp__('UV_ENOTDIR');
			case UV_ENOTEMPTY: untyped __cpp__('UV_ENOTEMPTY');
			case UV_ENOTSOCK: untyped __cpp__('UV_ENOTSOCK');
			case UV_ENOTSUP: untyped __cpp__('UV_ENOTSUP');
			case UV_EOVERFLOW: untyped __cpp__('UV_EOVERFLOW');
			case UV_EPERM: untyped __cpp__('UV_EPERM');
			case UV_EPIPE: untyped __cpp__('UV_EPIPE');
			case UV_EPROTO: untyped __cpp__('UV_EPROTO');
			case UV_EPROTONOSUPPORT: untyped __cpp__('UV_EPROTONOSUPPORT');
			case UV_EPROTOTYPE: untyped __cpp__('UV_EPROTOTYPE');
			case UV_ERANGE: untyped __cpp__('UV_ERANGE');
			case UV_EROFS: untyped __cpp__('UV_EROFS');
			case UV_ESHUTDOWN: untyped __cpp__('UV_ESHUTDOWN');
			case UV_ESPIPE: untyped __cpp__('UV_ESPIPE');
			case UV_ESRCH: untyped __cpp__('UV_ESRCH');
			case UV_ETIMEDOUT: untyped __cpp__('UV_ETIMEDOUT');
			case UV_ETXTBSY: untyped __cpp__('UV_ETXTBSY');
			case UV_EXDEV: untyped __cpp__('UV_EXDEV');
			case UV_UNKNOWN: untyped __cpp__('UV_UNKNOWN');
			case UV_EOF: untyped __cpp__('UV_EOF');
			case UV_ENXIO: untyped __cpp__('UV_ENXIO');
			case UV_EMLINK: untyped __cpp__('UV_EMLINK');
			case UV_ENOTTY: untyped __cpp__('UV_ENOTTY');
			case UV_EFTYPE: untyped __cpp__('UV_EFTYPE');
			case UV_EILSEQ: untyped __cpp__('UV_EILSEQ');
			case UV_ESOCKTNOSUPPORT: untyped __cpp__('UV_ESOCKTNOSUPPORT');
		}
	}
}