package asyncio;

import asyncio.CErrNo;

/**
	Error types
**/
@:using(asyncio.IoErrorType.IoErrorTypeTools)
enum IoErrorType {
	/** File or directory not found */
	FileNotFound;
	/** File or directory already exist */
	FileExist;
	/** No such process */
	ProcessNotFound;
	/** Permission denied */
	AccessDenied;
	/** The given path was not a directory as expected */
	NotDirectory;
	/** The given path is a directory, but a file was expected */
	IsDirectory;
	/** Too many open files */
	TooManyOpenFiles;
	/** Broken pipe */
	BrokenPipe;
	/** Directory not empty */
	NotEmpty;
	/** Requested address is not available */
	AddressNotAvailable;
	/** Connection reset by peer */
	ConnectionReset;
	/** Operation timed out */
	TimedOut;
	/** Connection refused */
	ConnectionRefused;
	/** Other errors from system calls described in <error.h> */
	CError(errNo:CErrNo);
	/** Any other error */
	CustomError(message:String);
}

class IoErrorTypeTools {
	/**
		Error type description
	**/
	static public function toString(type:IoErrorType):String {
		return switch type {
			case FileNotFound:
				"File or directory not found";
			case FileExist:
				"File or directory already exist";
			case ProcessNotFound:
				"No such process";
			case AccessDenied:
				"Permission denied";
			case NotDirectory:
				"The given path was not a directory as expected";
			case IsDirectory:
				"The given path is a directory, but a file was expected";
			case TooManyOpenFiles:
				"Too many open files";
			case BrokenPipe:
				"Broken pipe";
			case NotEmpty:
				"Directory not empty";
			case AddressNotAvailable:
				"Address already in use";
			case ConnectionReset:
				"Connection reset by peer";
			case TimedOut:
				"Operation timed out";
			case ConnectionRefused:
				"Connection refused";
			case CError(errNo):
				errNo.toString();
			case CustomError(message):
				message;
		}
	}
}
