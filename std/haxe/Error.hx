package haxe;

import asys.uv.UVErrorType;
import haxe.PosInfos;

/**
	Common class for errors.
**/
class Error {
	function get_message():String {
		return (switch (type) {
			case UVError(UVErrorType.E2BIG): "argument list too long";
			case UVError(UVErrorType.EACCES): "permission denied";
			case UVError(UVErrorType.EADDRINUSE): "address already in use";
			case UVError(UVErrorType.EADDRNOTAVAIL): "address not available";
			case UVError(UVErrorType.EAFNOSUPPORT): "address family not supported";
			case UVError(UVErrorType.EAGAIN): "resource temporarily unavailable";
			case UVError(UVErrorType.EAI_ADDRFAMILY): "address family not supported";
			case UVError(UVErrorType.EAI_AGAIN): "temporary failure";
			case UVError(UVErrorType.EAI_BADFLAGS): "bad ai_flags value";
			case UVError(UVErrorType.EAI_BADHINTS): "invalid value for hints";
			case UVError(UVErrorType.EAI_CANCELED): "request canceled";
			case UVError(UVErrorType.EAI_FAIL): "permanent failure";
			case UVError(UVErrorType.EAI_FAMILY): "ai_family not supported";
			case UVError(UVErrorType.EAI_MEMORY): "out of memory";
			case UVError(UVErrorType.EAI_NODATA): "no address";
			case UVError(UVErrorType.EAI_NONAME): "unknown node or service";
			case UVError(UVErrorType.EAI_OVERFLOW): "argument buffer overflow";
			case UVError(UVErrorType.EAI_PROTOCOL): "resolved protocol is unknown";
			case UVError(UVErrorType.EAI_SERVICE): "service not available for socket type";
			case UVError(UVErrorType.EAI_SOCKTYPE): "socket type not supported";
			case UVError(UVErrorType.EALREADY): "connection already in progress";
			case UVError(UVErrorType.EBADF): "bad file descriptor";
			case UVError(UVErrorType.EBUSY): "resource busy or locked";
			case UVError(UVErrorType.ECANCELED): "operation canceled";
			case UVError(UVErrorType.ECHARSET): "invalid Unicode character";
			case UVError(UVErrorType.ECONNABORTED): "software caused connection abort";
			case UVError(UVErrorType.ECONNREFUSED): "connection refused";
			case UVError(UVErrorType.ECONNRESET): "connection reset by peer";
			case UVError(UVErrorType.EDESTADDRREQ): "destination address required";
			case UVError(UVErrorType.EEXIST): "file already exists";
			case UVError(UVErrorType.EFAULT): "bad address in system call argument";
			case UVError(UVErrorType.EFBIG): "file too large";
			case UVError(UVErrorType.EHOSTUNREACH): "host is unreachable";
			case UVError(UVErrorType.EINTR): "interrupted system call";
			case UVError(UVErrorType.EINVAL): "invalid argument";
			case UVError(UVErrorType.EIO): "i/o error";
			case UVError(UVErrorType.EISCONN): "socket is already connected";
			case UVError(UVErrorType.EISDIR): "illegal operation on a directory";
			case UVError(UVErrorType.ELOOP): "too many symbolic links encountered";
			case UVError(UVErrorType.EMFILE): "too many open files";
			case UVError(UVErrorType.EMSGSIZE): "message too long";
			case UVError(UVErrorType.ENAMETOOLONG): "name too long";
			case UVError(UVErrorType.ENETDOWN): "network is down";
			case UVError(UVErrorType.ENETUNREACH): "network is unreachable";
			case UVError(UVErrorType.ENFILE): "file table overflow";
			case UVError(UVErrorType.ENOBUFS): "no buffer space available";
			case UVError(UVErrorType.ENODEV): "no such device";
			case UVError(UVErrorType.ENOENT): "no such file or directory";
			case UVError(UVErrorType.ENOMEM): "not enough memory";
			case UVError(UVErrorType.ENONET): "machine is not on the network";
			case UVError(UVErrorType.ENOPROTOOPT): "protocol not available";
			case UVError(UVErrorType.ENOSPC): "no space left on device";
			case UVError(UVErrorType.ENOSYS): "function not implemented";
			case UVError(UVErrorType.ENOTCONN): "socket is not connected";
			case UVError(UVErrorType.ENOTDIR): "not a directory";
			case UVError(UVErrorType.ENOTEMPTY): "directory not empty";
			case UVError(UVErrorType.ENOTSOCK): "socket operation on non-socket";
			case UVError(UVErrorType.ENOTSUP): "operation not supported on socket";
			case UVError(UVErrorType.EPERM): "operation not permitted";
			case UVError(UVErrorType.EPIPE): "broken pipe";
			case UVError(UVErrorType.EPROTO): "protocol error";
			case UVError(UVErrorType.EPROTONOSUPPORT): "protocol not supported";
			case UVError(UVErrorType.EPROTOTYPE): "protocol wrong type for socket";
			case UVError(UVErrorType.ERANGE): "result too large";
			case UVError(UVErrorType.EROFS): "read-only file system";
			case UVError(UVErrorType.ESHUTDOWN): "cannot send after transport endpoint shutdown";
			case UVError(UVErrorType.ESPIPE): "invalid seek";
			case UVError(UVErrorType.ESRCH): "no such process";
			case UVError(UVErrorType.ETIMEDOUT): "connection timed out";
			case UVError(UVErrorType.ETXTBSY): "text file is busy";
			case UVError(UVErrorType.EXDEV): "cross-device link not permitted";
			case UVError(UVErrorType.UNKNOWN): "unknown error";
			case UVError(UVErrorType.EOF): "end of file";
			case UVError(UVErrorType.ENXIO): "no such device or address";
			case UVError(UVErrorType.EMLINK): "too many links";
			case UVError(UVErrorType.EHOSTDOWN): "host is down";
			case UVError(UVErrorType.EOTHER): "other UV error";
			case _: "unknown error";
		});
	}

	/**
		A human-readable representation of the error.
	**/
	public var message(get, never):String;

	/**
		Position where the error was thrown. By default, this is the place where the error is constructed.
	**/
	public final posInfos:PosInfos;

	/**
		Error type, usable for discerning error types with `switch` statements.
	**/
	public final type:ErrorType;

	public function new(type:ErrorType, ?posInfos:PosInfos) {
		this.type = type;
		this.posInfos = posInfos;
	}

	public function toString():String {
		return '$message at $posInfos';
	}
}
