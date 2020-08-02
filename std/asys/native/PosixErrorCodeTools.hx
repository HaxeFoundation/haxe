package asys.native;

class PosixErrorCodeTools {
	/**
		Error description
	**/
	static public function toString(err:PosixErrorCode):String {
		return switch err {
			case E2BIG: "Argument list too long";
			case EACCES: "Permission denied";
			case EADDRINUSE: "Address already in use";
			case EADDRNOTAVAIL: "Address not available";
			case EAFNOSUPPORT: "Address family not supported";
			case EAGAIN: "Resource temporarily unavailable";
			case EALREADY: "Connection already in progress";
			case EBADE: "Invalid exchange";
			case EBADF: "Bad file descriptor";
			case EBADFD: "File descriptor in bad state";
			case EBADMSG: "Bad message";
			case EBADR: "Invalid request descriptor";
			case EBADRQC: "Invalid request code";
			case EBADSLT: "Invalid slot";
			case EBUSY: "Device or resource busy";
			case ECANCELED: "Operation canceled";
			case ECHILD: "No child processes";
			case ECHRNG: "Channel number out of range";
			case ECOMM: "Communication error on send";
			case ECONNABORTED: "Connection aborted";
			case ECONNREFUSED: "Connection refused";
			case ECONNRESET: "Connection reset";
			case EDEADLK: "Resource deadlock avoided";
			case EDESTADDRREQ: "Destination address required";
			case EDOM: "Mathematics argument out of domain of function";
			case EDQUOT: "Disk quota exceeded";
			case EEXIST: "File exists";
			case EFAULT: "Bad address";
			case EFBIG: "File too large";
			case EHOSTDOWN: "Host is down";
			case EHOSTUNREACH: "Host is unreachable";
			case EHWPOISON: "Memory page has hardware error";
			case EIDRM: "Identifier removed";
			case EILSEQ: "Invalid or incomplete multibyte or wide character";
			case EINPROGRESS: "Operation in progress";
			case EINTR: "Interrupted function call";
			case EINVAL: "Invalid argument";
			case EIO: "Input/output error";
			case EISCONN: "Socket is connected";
			case EISDIR: "Is a directory";
			case EISNAM: "Is a named type file";
			case EKEYEXPIRED: "Key has expired";
			case EKEYREJECTED: "Key was rejected by service";
			case EKEYREVOKED: "Key has been revoked";
			case EL2HLT: "Level 2 halted";
			case EL2NSYNC: "Level 2 not synchronized";
			case EL3HLT: "Level 3 halted";
			case EL3RST: "Level 3 reset";
			case ELIBACC: "Cannot access a needed shared library";
			case ELIBBAD: "Accessing a corrupted shared library";
			case ELIBMAX: "Attempting to link in too many shared libraries";
			case ELIBSCN: ".lib section in a.out corrupted";
			case ELIBEXEC: "Cannot exec a shared library directly";
			// case ELNRANGE: "Link number out of range";
			case ELOOP: "Too many levels of symbolic links";
			case EMEDIUMTYPE: "Wrong medium type";
			case EMFILE: "Too many open files";
			case EMLINK: "Too many links";
			case EMSGSIZE: "Message too long";
			case EMULTIHOP: "Multihop attempted";
			case ENAMETOOLONG: "Filename too long";
			case ENETDOWN: "Network is down";
			case ENETRESET: "Connection aborted by network";
			case ENETUNREACH: "Network unreachable";
			case ENFILE: "Too many open files in system";
			case ENOANO: "No anode";
			case ENOBUFS: "No buffer space available";
			case ENODATA: "No message is available on the STREAM head read queue";
			case ENODEV: "No such device";
			case ENOENT: "No such file or directory";
			case ENOEXEC: "Exec format error";
			case ENOKEY: "Required key not available";
			case ENOLCK: "No locks available";
			case ENOLINK: "Link has been severed";
			case ENOMEDIUM: "No medium found";
			case ENOMEM: "Not enough space/cannot allocate memory";
			case ENOMSG: "No message of the desired type";
			case ENONET: "Machine is not on the network";
			case ENOPKG: "Package not installed";
			case ENOPROTOOPT: "Protocol not available";
			case ENOSPC: "No space left on device";
			case ENOSR: "No STREAM resources";
			case ENOSTR: "Not a STREAM";
			case ENOSYS: "Function not implemented";
			case ENOTBLK: "Block device required";
			case ENOTCONN: "The socket is not connected";
			case ENOTDIR: "Not a directory";
			case ENOTEMPTY: "Directory not empty";
			case ENOTRECOVERABLE: " not recoverable";
			case ENOTSOCK: "Not a socket";
			case ENOTSUP: "Operation not supported";
			case ENOTTY: "Inappropriate I/O control operation";
			case ENOTUNIQ: "Name not unique on network";
			case ENXIO: "No such device or address";
			// case EOPNOTSUPP: "Operation not supported on socket";
			case EOVERFLOW: "Value too large to be stored in data type";
			case EOWNERDEAD: "Owner died";
			case EPERM: "Operation not permitted";
			case EPFNOSUPPORT: "Protocol family not supported";
			case EPIPE: "Broken pipe";
			case EPROTO: "Protocol error";
			case EPROTONOSUPPORT: " not supported";
			case EPROTOTYPE: "Protocol wrong type for socket";
			case ERANGE: "Result too large";
			case EREMCHG: "Remote address changed";
			case EREMOTE: "Object is remote";
			case EREMOTEIO: "Remote I/O error";
			case ERESTART: "Interrupted system call should be restarted";
			case ERFKILL: "Operation not possible due to RF-kill";
			case EROFS: "Read-only filesystem";
			case ESHUTDOWN: "Cannot send after transport endpoint shutdown";
			case ESPIPE: "Invalid seek";
			case ESOCKTNOSUPPORT: " type not supported";
			case ESRCH: "No such process";
			case ESTALE: "Stale file handle";
			case ESTRPIPE: "Streams pipe error";
			case ETIME: "Timer expired";
			case ETIMEDOUT: "Connection timed out";
			case ETOOMANYREFS: "Too many references: cannot splice";
			case ETXTBSY: "Text file busy";
			case EUCLEAN: "Structure needs cleaning";
			case EUNATCH: "Protocol driver not attached";
			case EUSERS: "Too many users";
			case EXDEV: "Improper link";
			case EXFULL: "Exchange full";
			case _: 'Error #$err';
		}
	}

	/**
		Convert error number to `asys.native.IoErrorType`
	**/
	static public function toIoErrorType(err:PosixErrorCode):IoErrorType {
		return switch err {
			case EPERM | EACCES: AccessDenied;
			case ENOENT: FileNotFound;
			case EEXIST: FileExists;
			case EISDIR: IsDirectory;
			case EMFILE: TooManyOpenFiles;
			case EPIPE: BrokenPipe;
			case ENOTEMPTY: NotEmpty;
			case EADDRINUSE | EADDRNOTAVAIL: AddressNotAvailable;
			case ECONNRESET: ConnectionReset;
			case ETIMEDOUT: TimedOut;
			case ECONNREFUSED: ConnectionRefused;
			case _: ErrorCode(err);
		}
	}
}