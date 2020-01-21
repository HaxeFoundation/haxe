package asyncio;

import asyncio.IoErrorType.IoErrorTypeTools;

/**
	Error numbers as described in <errno.h>.

	TODO: All the docs and strings below are copied from man errno. Is it legal?
**/
enum abstract CErrNo(Int) from Int to Int {
	/** Operation not permitted */
	var EPERM = 1;
	/** No such file or directory */
	var ENOENT = 2;
	/** No such process */
	var ESRCH = 3;
	/** Interrupted system call */
	var EINTR = 4;
	/** Input/output error */
	var EIO = 5;
	/** No such device or address */
	var ENXIO = 6;
	/** Argument list too long */
	var E2BIG = 7;
	/** Exec format error */
	var ENOEXEC = 8;
	/** Bad file descriptor */
	var EBADF = 9;
	/** No child processes */
	var ECHILD = 10;
	/** Resource temporarily unavailable */
	var EAGAIN = 11;
	/** Cannot allocate memory */
	var ENOMEM = 12;
	/** Permission denied */
	var EACCES = 13;
	/** Bad address */
	var EFAULT = 14;
	/** Block device required */
	var ENOTBLK = 15;
	/** Device or resource busy */
	var EBUSY = 16;
	/** File exists */
	var EEXIST = 17;
	/** Invalid cross-device link */
	var EXDEV = 18;
	/** No such device */
	var ENODEV = 19;
	/** Not a directory */
	var ENOTDIR = 20;
	/** Is a directory */
	var EISDIR = 21;
	/** Invalid argument */
	var EINVAL = 22;
	/** Too many open files in system */
	var ENFILE = 23;
	/** Too many open files */
	var EMFILE = 24;
	/** Inappropriate ioctl for device */
	var ENOTTY = 25;
	/** Text file busy */
	var ETXTBSY = 26;
	/** File too large */
	var EFBIG = 27;
	/** No space left on device */
	var ENOSPC = 28;
	/** Illegal seek */
	var ESPIPE = 29;
	/** Read-only file system */
	var EROFS = 30;
	/** Too many links */
	var EMLINK = 31;
	/** Broken pipe */
	var EPIPE = 32;
	/** Numerical argument out of domain */
	var EDOM = 33;
	/** Numerical result out of range */
	var ERANGE = 34;
	/** Resource deadlock avoided */
	var EDEADLK = 35;
	/** File name too long */
	var ENAMETOOLONG = 36;
	/** No locks available */
	var ENOLCK = 37;
	/** Function not implemented */
	var ENOSYS = 38;
	/** Directory not empty */
	var ENOTEMPTY = 39;
	/** Too many levels of symbolic links */
	var ELOOP = 40;
	/** Resource temporarily unavailable */
	var EWOULDBLOCK = 11;
	/** No message of desired type */
	var ENOMSG = 42;
	/** Identifier removed */
	var EIDRM = 43;
	/** Channel number out of range */
	var ECHRNG = 44;
	/** Level 2 not synchronized */
	var EL2NSYNC = 45;
	/** Level 3 halted */
	var EL3HLT = 46;
	/** Level 3 reset */
	var EL3RST = 47;
	/** Link number out of range */
	var ELNRNG = 48;
	/** Protocol driver not attached */
	var EUNATCH = 49;
	/** No CSI structure available */
	var ENOCSI = 50;
	/** Level 2 halted */
	var EL2HLT = 51;
	/** Invalid exchange */
	var EBADE = 52;
	/** Invalid request descriptor */
	var EBADR = 53;
	/** Exchange full */
	var EXFULL = 54;
	/** No anode */
	var ENOANO = 55;
	/** Invalid request code */
	var EBADRQC = 56;
	/** Invalid slot */
	var EBADSLT = 57;
	/** Resource deadlock avoided */
	var EDEADLOCK = 35;
	/** Bad font file format */
	var EBFONT = 59;
	/** Device not a stream */
	var ENOSTR = 60;
	/** No data available */
	var ENODATA = 61;
	/** Timer expired */
	var ETIME = 62;
	/** Out of streams resources */
	var ENOSR = 63;
	/** Machine is not on the network */
	var ENONET = 64;
	/** Package not installed */
	var ENOPKG = 65;
	/** Object is remote */
	var EREMOTE = 66;
	/** Link has been severed */
	var ENOLINK = 67;
	/** Advertise error */
	var EADV = 68;
	/** Srmount error */
	var ESRMNT = 69;
	/** Communication error on send */
	var ECOMM = 70;
	/** Protocol error */
	var EPROTO = 71;
	/** Multihop attempted */
	var EMULTIHOP = 72;
	/** RFS specific error */
	var EDOTDOT = 73;
	/** Bad message */
	var EBADMSG = 74;
	/** Value too large for defined data type */
	var EOVERFLOW = 75;
	/** Name not unique on network */
	var ENOTUNIQ = 76;
	/** File descriptor in bad state */
	var EBADFD = 77;
	/** Remote address changed */
	var EREMCHG = 78;
	/** Can not access a needed shared library */
	var ELIBACC = 79;
	/** Accessing a corrupted shared library */
	var ELIBBAD = 80;
	/** .lib section in a.out corrupted */
	var ELIBSCN = 81;
	/** Attempting to link in too many shared libraries */
	var ELIBMAX = 82;
	/** Cannot exec a shared library directly */
	var ELIBEXEC = 83;
	/** Invalid or incomplete multibyte or wide character */
	var EILSEQ = 84;
	/** Interrupted system call should be restarted */
	var ERESTART = 85;
	/** Streams pipe error */
	var ESTRPIPE = 86;
	/** Too many users */
	var EUSERS = 87;
	/** Socket operation on non-socket */
	var ENOTSOCK = 88;
	/** Destination address required */
	var EDESTADDRREQ = 89;
	/** Message too long */
	var EMSGSIZE = 90;
	/** Protocol wrong type for socket */
	var EPROTOTYPE = 91;
	/** Protocol not available */
	var ENOPROTOOPT = 92;
	/** Protocol not supported */
	var EPROTONOSUPPORT = 93;
	/** Socket type not supported */
	var ESOCKTNOSUPPORT = 94;
	/** Operation not supported */
	var EOPNOTSUPP = 95;
	/** Protocol family not supported */
	var EPFNOSUPPORT = 96;
	/** Address family not supported by protocol */
	var EAFNOSUPPORT = 97;
	/** Address already in use */
	var EADDRINUSE = 98;
	/** Cannot assign requested address */
	var EADDRNOTAVAIL = 99;
	/** Network is down */
	var ENETDOWN = 100;
	/** Network is unreachable */
	var ENETUNREACH = 101;
	/** Network dropped connection on reset */
	var ENETRESET = 102;
	/** Software caused connection abort */
	var ECONNABORTED = 103;
	/** Connection reset by peer */
	var ECONNRESET = 104;
	/** No buffer space available */
	var ENOBUFS = 105;
	/** Transport endpoint is already connected */
	var EISCONN = 106;
	/** Transport endpoint is not connected */
	var ENOTCONN = 107;
	/** Cannot send after transport endpoint shutdown */
	var ESHUTDOWN = 108;
	/** Too many references: cannot splice */
	var ETOOMANYREFS = 109;
	/** Connection timed out */
	var ETIMEDOUT = 110;
	/** Connection refused */
	var ECONNREFUSED = 111;
	/** Host is down */
	var EHOSTDOWN = 112;
	/** No route to host */
	var EHOSTUNREACH = 113;
	/** Operation already in progress */
	var EALREADY = 114;
	/** Operation now in progress */
	var EINPROGRESS = 115;
	/** Stale file handle */
	var ESTALE = 116;
	/** Structure needs cleaning */
	var EUCLEAN = 117;
	/** Not a XENIX named type file */
	var ENOTNAM = 118;
	/** No XENIX semaphores available */
	var ENAVAIL = 119;
	/** Is a named type file */
	var EISNAM = 120;
	/** Remote I/O error */
	var EREMOTEIO = 121;
	/** Disk quota exceeded */
	var EDQUOT = 122;
	/** No medium found */
	var ENOMEDIUM = 123;
	/** Wrong medium type */
	var EMEDIUMTYPE = 124;
	/** Operation canceled */
	var ECANCELED = 125;
	/** Required key not available */
	var ENOKEY = 126;
	/** Key has expired */
	var EKEYEXPIRED = 127;
	/** Key has been revoked */
	var EKEYREVOKED = 128;
	/** Key was rejected by service */
	var EKEYREJECTED = 129;
	/** Owner died */
	var EOWNERDEAD = 130;
	/** State not recoverable */
	var ENOTRECOVERABLE = 131;
	/** Operation not possible due to RF-kill */
	var ERFKILL = 132;
	/** Memory page has hardware error */
	var EHWPOISON = 133;
	/** Operation not supported */
	var ENOTSUP = 95;

	/**
		Error description
	**/
	public function toString():String {
		return switch this {
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
			case _: 'Error #$this';
		}
	}

	/**
		Convert C error number to `aio.ErrorType`
	**/
	@:to public function toIoErrorType():IoErrorType {
		return switch this {
			case EPERM | EACCES: AccessDenied;
			case ENOENT: FileNotFound;
			case EEXIST: FileExist;
			case EISDIR: IsDirectory;
			case EMFILE: TooManyOpenFiles;
			case EPIPE: BrokenPipe;
			case ENOTEMPTY: NotEmpty;
			case EADDRINUSE | EADDRNOTAVAIL: AddressNotAvailable;
			case ECONNRESET: ConnectionReset;
			case ETIMEDOUT: TimedOut;
			case ECONNREFUSED: ConnectionRefused;
			case _: CError(this);
		}
	}
}