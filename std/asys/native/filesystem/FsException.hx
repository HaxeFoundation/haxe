package asys.native.filesystem;

import haxe.Exception;

/**
	File system errors.

	TODO: `FileSystemException` is probably a better name
**/
class FsException extends IoException {
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
	override function get_message():String {
		return super.get_message() + ' on ${path.toReadableString()}';
	}
}