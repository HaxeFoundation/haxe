package asyncio.filesystem.errors;

import haxe.PosInfos;

/**
	File system errors.
**/
class FsError extends IoError {
	/**
		A target path of a failed operation.
	**/
	public final path:FilePath;

	public function new(type:IoErrorType, path:FilePath, ?pos:PosInfos) {
		super(type, pos);
		this.path = path;
	}

	/**
		Error description.
	**/
	override function toString():String {
		return 'Error "${type.toString()}" on ${path.toReadableString()} at ${pos()}';
	}
}