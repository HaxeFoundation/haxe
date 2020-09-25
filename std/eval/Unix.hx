package eval;

/**
	This class provides access to functions of OCaml module `Unix`
	@see https://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html
**/
extern class Unix {
	/**
		Check if provided `path` is a symbolic link.
	**/
	extern static public function isLink(path:NativeString):Bool;

	/**
		String describing the error.
	**/
	static public inline function errorMessage(e:PosixError):String {
		return e.toString();
	}
}

/**
	The type of error codes as defined in the POSIX standard and additional errors
	from UNIX98 and BSD. All other errors are mapped to EUNKNOWNERR.
**/
@:using(eval.Unix.PosixErrorTools)
enum PosixError {
	/** Argument list too long */
	E2BIG;
	/** Permission denied */
	EACCES;
	/** Resource temporarily unavailable; try again */
	EAGAIN;
	/** Bad file descriptor */
	EBADF;
	/** Resource unavailable */
	EBUSY;
	/** No child process */
	ECHILD;
	/** Resource deadlock would occur */
	EDEADLK;
	/** Domain error for math functions, etc. */
	EDOM;
	/** File exists */
	EEXIST;
	/** Bad address */
	EFAULT;
	/** File too large */
	EFBIG;
	/** Function interrupted by signal */
	EINTR;
	/** Invalid argument */
	EINVAL;
	/** Hardware I/O error */
	EIO;
	/** Is a directory */
	EISDIR;
	/** Too many open files by the process */
	EMFILE;
	/** Too many links */
	EMLINK;
	/** Filename too long */
	ENAMETOOLONG;
	/** Too many open files in the system */
	ENFILE;
	/** No such device */
	ENODEV;
	/** No such file or directory */
	ENOENT;
	/** Not an executable file */
	ENOEXEC;
	/** No locks available */
	ENOLCK;
	/** Not enough memory */
	ENOMEM;
	/** No space left on device */
	ENOSPC;
	/** Function not supported */
	ENOSYS;
	/** Not a directory */
	ENOTDIR;
	/** Directory not empty */
	ENOTEMPTY;
	/** Inappropriate I/O control operation */
	ENOTTY;
	/** No such device or address */
	ENXIO;
	/** Operation not permitted */
	EPERM;
	/** Broken pipe */
	EPIPE;
	/** Result too large */
	ERANGE;
	/** Read-only file system */
	EROFS;
	/** Invalid seek e.g. on a pipe */
	ESPIPE;
	/** No such process */
	ESRCH;
	/** Invalid link */
	EXDEV;
	/** Operation would block */
	EWOULDBLOCK;
	/** Operation now in progress */
	EINPROGRESS;
	/** Operation already in progress */
	EALREADY;
	/** Socket operation on non-socket */
	ENOTSOCK;
	/** Destination address required */
	EDESTADDRREQ;
	/** Message too long */
	EMSGSIZE;
	/** Protocol wrong type for socket */
	EPROTOTYPE;
	/** Protocol not available */
	ENOPROTOOPT;
	/** Protocol not supported */
	EPROTONOSUPPORT;
	/** Socket type not supported */
	ESOCKTNOSUPPORT;
	/** Operation not supported on socket */
	EOPNOTSUPP;
	/** Protocol family not supported */
	EPFNOSUPPORT;
	/** Address family not supported by protocol family */
	EAFNOSUPPORT;
	/** Address already in use */
	EADDRINUSE;
	/** Can't assign requested address */
	EADDRNOTAVAIL;
	/** Network is down */
	ENETDOWN;
	/** Network is unreachable */
	ENETUNREACH;
	/** Network dropped connection on reset */
	ENETRESET;
	/** Software caused connection abort */
	ECONNABORTED;
	/** Connection reset by peer */
	ECONNRESET;
	/** No buffer space available */
	ENOBUFS;
	/** Socket is already connected */
	EISCONN;
	/** Socket is not connected */
	ENOTCONN;
	/** Can't send after socket shutdown */
	ESHUTDOWN;
	/** Too many references: can't splice */
	ETOOMANYREFS;
	/** Connection timed out */
	ETIMEDOUT;
	/** Connection refused */
	ECONNREFUSED;
	/** Host is down */
	EHOSTDOWN;
	/** No route to host */
	EHOSTUNREACH;
	/** Too many levels of symbolic links */
	ELOOP;
	/** File size or position not representable */
	EOVERFLOW;
	/** Unknown error */
	EUNKNOWNERR(code:Int);
}

private class PosixErrorTools {
	static public function toString(e:PosixError):String {
		return switch e {
			case E2BIG: "Argument list too long";
			case EACCES: "Permission denied";
			case EAGAIN: "Resource temporarily unavailable; try again";
			case EBADF: "Bad file descriptor";
			case EBUSY: "Resource unavailable";
			case ECHILD: "No child process";
			case EDEADLK: "Resource deadlock would occur";
			case EDOM: "Domain error for math functions, etc.";
			case EEXIST: "File exists";
			case EFAULT: "Bad address";
			case EFBIG: "File too large";
			case EINTR: "Function interrupted by signal";
			case EINVAL: "Invalid argument";
			case EIO: "Hardware I/O error";
			case EISDIR: "Is a directory";
			case EMFILE: "Too many open files by the process";
			case EMLINK: "Too many links";
			case ENAMETOOLONG: "Filename too long";
			case ENFILE: "Too many open files in the system";
			case ENODEV: "No such device";
			case ENOENT: "No such file or directory";
			case ENOEXEC: "Not an executable file";
			case ENOLCK: "No locks available";
			case ENOMEM: "Not enough memory";
			case ENOSPC: "No space left on device";
			case ENOSYS: "Function not supported";
			case ENOTDIR: "Not a directory";
			case ENOTEMPTY: "Directory not empty";
			case ENOTTY: "Inappropriate I/O control operation";
			case ENXIO: "No such device or address";
			case EPERM: "Operation not permitted";
			case EPIPE: "Broken pipe";
			case ERANGE: "Result too large";
			case EROFS: "Read-only file system";
			case ESPIPE: "Invalid seek e.g. on a pipe";
			case ESRCH: "No such process";
			case EXDEV: "Invalid link";
			case EWOULDBLOCK: "Operation would block";
			case EINPROGRESS: "Operation now in progress";
			case EALREADY: "Operation already in progress";
			case ENOTSOCK: "Socket operation on non-socket";
			case EDESTADDRREQ: "Destination address required";
			case EMSGSIZE: "Message too long";
			case EPROTOTYPE: "Protocol wrong type for socket";
			case ENOPROTOOPT: "Protocol not available";
			case EPROTONOSUPPORT: "Protocol not supported";
			case ESOCKTNOSUPPORT: "Socket type not supported";
			case EOPNOTSUPP: "Operation not supported on socket";
			case EPFNOSUPPORT: "Protocol family not supported";
			case EAFNOSUPPORT: "Address family not supported by protocol family";
			case EADDRINUSE: "Address already in use";
			case EADDRNOTAVAIL: "Can't assign requested address";
			case ENETDOWN: "Network is down";
			case ENETUNREACH: "Network is unreachable";
			case ENETRESET: "Network dropped connection on reset";
			case ECONNABORTED: "Software caused connection abort";
			case ECONNRESET: "Connection reset by peer";
			case ENOBUFS: "No buffer space available";
			case EISCONN: "Socket is already connected";
			case ENOTCONN: "Socket is not connected";
			case ESHUTDOWN: "Can't send after socket shutdown";
			case ETOOMANYREFS: "Too many references: can't splice";
			case ETIMEDOUT: "Connection timed out";
			case ECONNREFUSED: "Connection refused";
			case EHOSTDOWN: "Host is down";
			case EHOSTUNREACH: "No route to host";
			case ELOOP: "Too many levels of symbolic links";
			case EOVERFLOW: "File size or position not representable";
			case EUNKNOWNERR(code): "Error #" + code;
		}
	}
}