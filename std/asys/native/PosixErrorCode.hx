package asys.native;

import asys.native.IoErrorType.IoErrorTypeTools;

/**
	Error numbers as described in <errno.h>.

	TODO:
	All the docs and strings below are copied from man errno.
	Rewrite to avoid legal issues.
**/
@:using(asys.native.PosixErrorCodeTools)
extern enum abstract PosixErrorCode(Int) from Int to Int {
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