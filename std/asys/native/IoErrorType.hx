package asys.native;

/**
	Error types.

	TODO: add more error types
**/
@:using(asys.native.IoErrorType.IoErrorTypeTools)
enum IoErrorType {
	/** File or directory not found */
	FileNotFound;
	/** File or directory already exist */
	FileExists;
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
	/** Bad file descriptor */
	BadFile;
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
			case FileExists:
				"File or directory already exists";
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
			case BadFile:
				"Bad file descriptor";
			case CustomError(message):
				message;
		}
	}
}
