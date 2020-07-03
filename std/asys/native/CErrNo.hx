package asys.native;

import asys.native.IoErrorType.IoErrorTypeTools;

/**
	Error numbers as described in <errno.h>.

	TODO:
	All the docs and strings below are copied from man errno.
	Rewrite to avoid legal issues.
**/
@:using(asys.native.CErrNo.CErrNoTools)
extern enum abstract CErrNo(Int) from Int to Int {
	/** Operation not permitted */
	var EPERM;
	/** No such file or directory */
	var ENOENT;
	/** No such process */
	var ESRCH;
	/** Interrupted system call */
	var EINTR;
	/** Input/output error */
	var EIO;
	/** No such device or address */
	var ENXIO;
	/** Argument list too long */
	var E2BIG;
	/** Exec format error */
	var ENOEXEC;
	/** Bad file descriptor */
	var EBADF;
	/** No child processes */
	var ECHILD;
	/** Resource temporarily unavailable */
	var EAGAIN;
	/** Cannot allocate memory */
	var ENOMEM;
	/** Permission denied */
	var EACCES;
	/** Bad address */
	var EFAULT;
	/** Block device required */
	var ENOTBLK;
	/** Device or resource busy */
	var EBUSY;
	/** File exists */
	var EEXIST;
	/** Invalid cross-device link */
	var EXDEV;
	/** No such device */
	var ENODEV;
	/** Not a directory */
	var ENOTDIR;
	/** Is a directory */
	var EISDIR;
	/** Invalid argument */
	var EINVAL;
	/** Too many open files in system */
	var ENFILE;
	/** Too many open files */
	var EMFILE;
	/** Inappropriate ioctl for device */
	var ENOTTY;
	/** Text file busy */
	var ETXTBSY;
	/** File too large */
	var EFBIG;
	/** No space left on device */
	var ENOSPC;
	/** Illegal seek */
	var ESPIPE;
	/** Read-only file system */
	var EROFS;
	/** Too many links */
	var EMLINK;
	/** Broken pipe */
	var EPIPE;
	/** Numerical argument out of domain */
	var EDOM;
	/** Numerical result out of range */
	var ERANGE;
	/** Resource deadlock avoided */
	var EDEADLK;
	/** File name too long */
	var ENAMETOOLONG;
	/** No locks available */
	var ENOLCK;
	/** Function not implemented */
	var ENOSYS;
	/** Directory not empty */
	var ENOTEMPTY;
	/** Too many levels of symbolic links */
	var ELOOP;
	/** Resource temporarily unavailable */
	var EWOULDBLOCK;
	/** No message of desired type */
	var ENOMSG;
	/** Identifier removed */
	var EIDRM;
	/** Channel number out of range */
	var ECHRNG;
	/** Level 2 not synchronized */
	var EL2NSYNC;
	/** Level 3 halted */
	var EL3HLT;
	/** Level 3 reset */
	var EL3RST;
	/** Link number out of range */
	var ELNRNG;
	/** Protocol driver not attached */
	var EUNATCH;
	/** No CSI structure available */
	var ENOCSI;
	/** Level 2 halted */
	var EL2HLT;
	/** Invalid exchange */
	var EBADE;
	/** Invalid request descriptor */
	var EBADR;
	/** Exchange full */
	var EXFULL;
	/** No anode */
	var ENOANO;
	/** Invalid request code */
	var EBADRQC;
	/** Invalid slot */
	var EBADSLT;
	/** Resource deadlock avoided */
	var EDEADLOCK;
	/** Bad font file format */
	var EBFONT;
	/** Device not a stream */
	var ENOSTR;
	/** No data available */
	var ENODATA;
	/** Timer expired */
	var ETIME;
	/** Out of streams resources */
	var ENOSR;
	/** Machine is not on the network */
	var ENONET;
	/** Package not installed */
	var ENOPKG;
	/** Object is remote */
	var EREMOTE;
	/** Link has been severed */
	var ENOLINK;
	/** Advertise error */
	var EADV;
	/** Srmount error */
	var ESRMNT;
	/** Communication error on send */
	var ECOMM;
	/** Protocol error */
	var EPROTO;
	/** Multihop attempted */
	var EMULTIHOP;
	/** RFS specific error */
	var EDOTDOT;
	/** Bad message */
	var EBADMSG;
	/** Value too large for defined data type */
	var EOVERFLOW;
	/** Name not unique on network */
	var ENOTUNIQ;
	/** File descriptor in bad state */
	var EBADFD;
	/** Remote address changed */
	var EREMCHG;
	/** Can not access a needed shared library */
	var ELIBACC;
	/** Accessing a corrupted shared library */
	var ELIBBAD;
	/** .lib section in a.out corrupted */
	var ELIBSCN;
	/** Attempting to link in too many shared libraries */
	var ELIBMAX;
	/** Cannot exec a shared library directly */
	var ELIBEXEC;
	/** Invalid or incomplete multibyte or wide character */
	var EILSEQ;
	/** Interrupted system call should be restarted */
	var ERESTART;
	/** Streams pipe error */
	var ESTRPIPE;
	/** Too many users */
	var EUSERS;
	/** Socket operation on non-socket */
	var ENOTSOCK;
	/** Destination address required */
	var EDESTADDRREQ;
	/** Message too long */
	var EMSGSIZE;
	/** Protocol wrong type for socket */
	var EPROTOTYPE;
	/** Protocol not available */
	var ENOPROTOOPT;
	/** Protocol not supported */
	var EPROTONOSUPPORT;
	/** Socket type not supported */
	var ESOCKTNOSUPPORT;
	/** Operation not supported */
	var EOPNOTSUPP;
	/** Protocol family not supported */
	var EPFNOSUPPORT;
	/** Address family not supported by protocol */
	var EAFNOSUPPORT;
	/** Address already in use */
	var EADDRINUSE;
	/** Cannot assign requested address */
	var EADDRNOTAVAIL;
	/** Network is down */
	var ENETDOWN;
	/** Network is unreachable */
	var ENETUNREACH;
	/** Network dropped connection on reset */
	var ENETRESET;
	/** Software caused connection abort */
	var ECONNABORTED;
	/** Connection reset by peer */
	var ECONNRESET;
	/** No buffer space available */
	var ENOBUFS;
	/** Transport endpoint is already connected */
	var EISCONN;
	/** Transport endpoint is not connected */
	var ENOTCONN;
	/** Cannot send after transport endpoint shutdown */
	var ESHUTDOWN;
	/** Too many references: cannot splice */
	var ETOOMANYREFS;
	/** Connection timed out */
	var ETIMEDOUT;
	/** Connection refused */
	var ECONNREFUSED;
	/** Host is down */
	var EHOSTDOWN;
	/** No route to host */
	var EHOSTUNREACH;
	/** Operation already in progress */
	var EALREADY;
	/** Operation now in progress */
	var EINPROGRESS;
	/** Stale file handle */
	var ESTALE;
	/** Structure needs cleaning */
	var EUCLEAN;
	/** Not a XENIX named type file */
	var ENOTNAM;
	/** No XENIX semaphores available */
	var ENAVAIL;
	/** Is a named type file */
	var EISNAM;
	/** Remote I/O error */
	var EREMOTEIO;
	/** Disk quota exceeded */
	var EDQUOT;
	/** No medium found */
	var ENOMEDIUM;
	/** Wrong medium type */
	var EMEDIUMTYPE;
	/** Operation canceled */
	var ECANCELED;
	/** Required key not available */
	var ENOKEY;
	/** Key has expired */
	var EKEYEXPIRED;
	/** Key has been revoked */
	var EKEYREVOKED;
	/** Key was rejected by service */
	var EKEYREJECTED;
	/** Owner died */
	var EOWNERDEAD;
	/** State not recoverable */
	var ENOTRECOVERABLE;
	/** Operation not possible due to RF-kill */
	var ERFKILL;
	/** Memory page has hardware error */
	var EHWPOISON;
	/** Operation not supported */
	var ENOTSUP;
}

class CErrNoTools {
	/**
		Error description
	**/
	static public function toString(err:CErrNo):String {
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
		Convert C error number to `asys.native.IoErrorType`
	**/
	static public function toIoErrorType(err:CErrNo):IoErrorType {
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
			case _: CError(err);
		}
	}
}