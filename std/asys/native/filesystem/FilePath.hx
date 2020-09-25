package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;

private typedef NativeFilePath = Dynamic;

/**
	Represents a relative or absolute file path.

	TODO: add API from `haxe.io.Path`
**/
@:coreApi abstract FilePath(NativeFilePath) {
	/**
		Standard directory separator character for current platform.
		E.g. `\\` for Windows or `/` for Unix-like systems.
	**/
	public static var SEPARATOR(get,never):String;
	static function get_SEPARATOR():String {
		return Sys.systemName() == 'Windows' ? '\\' : '/';
	}

	/**
		Create file path from plain string.
		Removes trailing slashes.
	**/
	@:from public static function fromString(path:String):FilePath {
		throw new NotImplementedException();
	}

	/**
		Get string representation of this path.
	**/
	@:to public function toString():String {
		throw new NotImplementedException();
	}

	@:op(A == B) function equals(p:FilePath):Bool {
		throw new NotImplementedException();
	}

	/**
		Get an absolute path of this path.
		For example translates `./path` to `/current/dir/path`.
		Resolves `.` and `..` and removes excessive slashes.
		Does not resolve symbolic links.
		It does not matter if the path does not exist.
	**/
	public function absolute():FilePath {
		throw new NotImplementedException();
	}
}