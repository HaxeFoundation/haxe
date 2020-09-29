package eval;

import haxe.io.Bytes;

/**
	This class provides access to functions of OCaml module `Unix` and other related functions.
	@see https://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html
**/
extern class Unix {
	/**
		String describing the error.
	**/
	static public inline function errorMessage(e:PosixError):String {
		return e.toString();
	}

	/**
		Check if provided `path` is a symbolic link.
	**/
	extern static public function isLink(path:NativeString):Bool;

	/**
		Check if provided `path` is a regular file.
	**/
	extern static public function isFile(path:NativeString):Bool;

	/**
		Check if provided `path` is a directory.
	**/
	extern static public function isDirectory(path:NativeString):Bool;

	/**
		Get file status.
		If `path` is a symbolic link it will be followed.
	**/
	extern static public function stat(path:NativeString):Stats;

	/**
		Get file status.
		Does not follow symbolic links
	**/
	extern static public function lstat(path:NativeString):Stats;

	/**
		Set file access time and last modification time.
		Times are represented as unix timestamp (amount of seconds since 1970-01-01 00:00:00).
		If both times are `0.0` then the access time and modification time are set
		to the current time.
	**/
	extern static public function utimes(path:NativeString, accessTime:Float, modificationTime:Float):Void;

	/**
		Obtain canonicalized absolute path.
	**/
	extern static public function realPath(path:NativeString):NativeString;

	/**
		Create a directory with the given permissions.
	**/
	extern static public function mkdir(path:NativeString, permissions:Int, recursive:Bool):Void;

	/**
		Open the file with the given flags.
		Set file `permissions` if the file is created.
	**/
	extern static public function openFile(path:NativeString, flags:Array<OpenFlag>, permissions:Int):FileDescriptor;

	/**
		Close the file.
	**/
	extern static public function closeFile(file:FileDescriptor):Void;

	/**
		Read up to `length` bytes from `file` and store them in `buffer` starting
		at position `pos`.
		Returns amount of bytes actually read.
	**/
	extern static public function read(file:FileDescriptor, buffer:Bytes, pos:Int, length:Int):Int;

	/**
		Writes `length` bytes to `file` taking them from `buffer` starting at
		position `pos`.
		Returns amount of bytes actually written.
		This method repeats writing operation until all bytes have been written or
		an error occurs.
	**/
	extern static public function write(file:FileDescriptor, buffer:Bytes, pos:Int, length:Int):Int;
}

@:coreType abstract FileDescriptor {}

enum abstract OpenFlag(Int) {
	/** Open for reading */
	var O_RDONLY = 0;
	/** Open for writing */
	var O_WRONLY = 1;
	/** Open for reading and writing */
	var O_RDWR = 2;
	/** Open in non-blocking mode */
	var O_NONBLOCK = 3;
	/** Open for append */
	var O_APPEND = 4;
	/** Create if nonexistent */
	var O_CREAT = 5;
	/** Truncate to 0 length if existing */
	var O_TRUNC = 6;
	/** Fail if existing */
	var O_EXCL = 7;
	/** Don't make this dev a controlling tty */
	var O_NOCTTY = 8;
	/** Writes complete as `Synchronised I/O data integrity completion' */
	var O_DSYNC = 9;
	/** Writes complete as `Synchronised I/O file integrity completion' */
	var O_SYNC = 10;
	/** Reads complete as writes (depending on O_SYNC/O_DSYNC) */
	var O_RSYNC = 11;
	/** Windows only: allow the file to be deleted while still open */
	var O_SHARE_DELETE = 11;
	/** Set the close-on-exec flag on the descriptor returned by `openFile`. */
	var O_CLOEXEC = 12;
	/** Clear the close-on-exec flag. This is currently the default. */
	var O_KEEPEXEC = 13;
}

enum abstract FileKind(Int) {
	/** Regular file */
	var S_REG = 0;
	/** Directory */
	var S_DIR = 1;
	/** Character device */
	var S_CHR = 2;
	/** Block device */
	var S_BLK = 3;
	/** Symbolic link */
	var S_LNK = 4;
	/** Named pipe */
	var S_FIFO = 5;
	/** Socket */
	var S_SOCK = 6;
}

typedef Stats = {
	/** Device number */
	var st_dev:Int;
	/** Inode number */
	var st_ino:Int;
	/** Kind of the file */
	var st_kind:FileKind;
	/** Access rights */
	var st_perm:Int;
	/** Number of links */
	var st_nlink:Int;
	/** User id of the owner */
	var st_uid:Int;
	/** Group ID of the file's group */
	var st_gid:Int;
	/** Device minor number */
	var st_rdev:Int;
	/** Size in bytes */
	var st_size:Int;
	/** Last access time */
	var st_atime:Int;
	/** Last modification time */
	var st_mtime:Int;
	/** Last status change time */
	var st_ctime:Int;
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