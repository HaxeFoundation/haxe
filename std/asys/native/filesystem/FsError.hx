package asys.native.filesystem;

import haxe.Exception;

/**
	File system errors.
**/
class FsError extends IoError {
	/**
		A target path of a failed operation.
	**/
	public final path:FilePath;

	public function new(type:IoErrorType, path:FilePath, ?previous:Exception) {
		super(type, previous);
		this.path = path;
	}

	/**
		Error description.
	**/
	override function toString():String {
		return 'Error "$message" on ${path.toReadableString()}';
	}
}