package asys.uv;

extern enum abstract UVErrorType(Int) {
	/**
		Argument list too long.
	**/
	var E2BIG;

	/**
		Permission denied.
	**/
	var EACCES;

	/**
		Address already in use.
	**/
	var EADDRINUSE;

	/**
		Address not available.
	**/
	var EADDRNOTAVAIL;

	/**
		Address family not supported.
	**/
	var EAFNOSUPPORT;

	/**
		Resource temporarily unavailable.
	**/
	var EAGAIN;

	/**
		Address family not supported.
	**/
	var EAI_ADDRFAMILY;

	/**
		Temporary failure.
	**/
	var EAI_AGAIN;

	/**
		Bad ai_flags value.
	**/
	var EAI_BADFLAGS;

	/**
		Invalid value for hints.
	**/
	var EAI_BADHINTS;

	/**
		Request canceled.
	**/
	var EAI_CANCELED;

	/**
		Permanent failure.
	**/
	var EAI_FAIL;

	/**
		Ai_family not supported.
	**/
	var EAI_FAMILY;

	/**
		Out of memory.
	**/
	var EAI_MEMORY;

	/**
		No address.
	**/
	var EAI_NODATA;

	/**
		Unknown node or service.
	**/
	var EAI_NONAME;

	/**
		Argument buffer overflow.
	**/
	var EAI_OVERFLOW;

	/**
		Resolved protocol is unknown.
	**/
	var EAI_PROTOCOL;

	/**
		Service not available for socket type.
	**/
	var EAI_SERVICE;

	/**
		Socket type not supported.
	**/
	var EAI_SOCKTYPE;

	/**
		Connection already in progress.
	**/
	var EALREADY;

	/**
		Bad file descriptor.
	**/
	var EBADF;

	/**
		Resource busy or locked.
	**/
	var EBUSY;

	/**
		Operation canceled.
	**/
	var ECANCELED;

	/**
		Invalid Unicode character.
	**/
	var ECHARSET;

	/**
		Software caused connection abort.
	**/
	var ECONNABORTED;

	/**
		Connection refused.
	**/
	var ECONNREFUSED;

	/**
		Connection reset by peer.
	**/
	var ECONNRESET;

	/**
		Destination address required.
	**/
	var EDESTADDRREQ;

	/**
		File already exists.
	**/
	var EEXIST;

	/**
		Bad address in system call argument.
	**/
	var EFAULT;

	/**
		File too large.
	**/
	var EFBIG;

	/**
		Host is unreachable.
	**/
	var EHOSTUNREACH;

	/**
		Interrupted system call.
	**/
	var EINTR;

	/**
		Invalid argument.
	**/
	var EINVAL;

	/**
		I/o error.
	**/
	var EIO;

	/**
		Socket is already connected.
	**/
	var EISCONN;

	/**
		Illegal operation on a directory.
	**/
	var EISDIR;

	/**
		Too many symbolic links encountered.
	**/
	var ELOOP;

	/**
		Too many open files.
	**/
	var EMFILE;

	/**
		Message too long.
	**/
	var EMSGSIZE;

	/**
		Name too long.
	**/
	var ENAMETOOLONG;

	/**
		Network is down.
	**/
	var ENETDOWN;

	/**
		Network is unreachable.
	**/
	var ENETUNREACH;

	/**
		File table overflow.
	**/
	var ENFILE;

	/**
		No buffer space available.
	**/
	var ENOBUFS;

	/**
		No such device.
	**/
	var ENODEV;

	/**
		No such file or directory.
	**/
	var ENOENT;

	/**
		Not enough memory.
	**/
	var ENOMEM;

	/**
		Machine is not on the network.
	**/
	var ENONET;

	/**
		Protocol not available.
	**/
	var ENOPROTOOPT;

	/**
		No space left on device.
	**/
	var ENOSPC;

	/**
		Function not implemented.
	**/
	var ENOSYS;

	/**
		Socket is not connected.
	**/
	var ENOTCONN;

	/**
		Not a directory.
	**/
	var ENOTDIR;

	/**
		Directory not empty.
	**/
	var ENOTEMPTY;

	/**
		Socket operation on non-socket.
	**/
	var ENOTSOCK;

	/**
		Operation not supported on socket.
	**/
	var ENOTSUP;

	/**
		Operation not permitted.
	**/
	var EPERM;

	/**
		Broken pipe.
	**/
	var EPIPE;

	/**
		Protocol error.
	**/
	var EPROTO;

	/**
		Protocol not supported.
	**/
	var EPROTONOSUPPORT;

	/**
		Protocol wrong type for socket.
	**/
	var EPROTOTYPE;

	/**
		Result too large.
	**/
	var ERANGE;

	/**
		Read-only file system.
	**/
	var EROFS;

	/**
		Cannot send after transport endpoint shutdown.
	**/
	var ESHUTDOWN;

	/**
		Invalid seek.
	**/
	var ESPIPE;

	/**
		No such process.
	**/
	var ESRCH;

	/**
		Connection timed out.
	**/
	var ETIMEDOUT;

	/**
		Text file is busy.
	**/
	var ETXTBSY;

	/**
		Cross-device link not permitted.
	**/
	var EXDEV;

	/**
		Unknown error.
	**/
	var UNKNOWN;

	/**
		End of file.
	**/
	var EOF;

	/**
		No such device or address.
	**/
	var ENXIO;

	/**
		Too many links.
	**/
	var EMLINK;

	/**
		Host is down.
	**/
	var EHOSTDOWN;

	/**
		Unknown error within libuv or libuv glue code.
	**/
	var EOTHER;
}
