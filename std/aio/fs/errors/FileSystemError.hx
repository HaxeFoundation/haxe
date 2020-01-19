package aio.fs.errors;

import haxe.PosInfos;
import haxe.Error;

/**
	Base class for file system errors.
**/
class FileSystemError extends Error {
	/**
		A target path of a failed operation.
	**/
	public final path:FilePath;

	public function new(path:FilePath, message:String, ?pos:PosInfos) {
		super(message, pos);
		this.path = path;
	}
}