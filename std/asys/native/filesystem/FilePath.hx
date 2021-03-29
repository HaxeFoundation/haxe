package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;

private typedef NativeFilePath = Dynamic;

/**
	Represents a relative or absolute file path.

	File path cannot be empty.
	E.g. creating a path using empty string produces a path of `.`.

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

		Creates a path of `.` if `path` is empty.
		That is `FilePath.ofString('') == FilePath.ofString('.')`.
	**/
	@:from public static function ofString(path:String):FilePath {
		throw new NotImplementedException();
	}

	/**
		Get string representation of this path.
	**/
	@:to public function toString():String {
		throw new NotImplementedException();
	}

	/**
		TODO: Should `my/path` and `my\\path` be equal on windows?
	**/
	@:op(A == B) function equals(p:FilePath):Bool {
		throw new NotImplementedException();
	}

	/**
		Check if this is an absolute path.
	**/
	public function isAbsolute():Bool {
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

	/**
		Get the parent element of this path.
		E.g. for `dir/to/path` this method returns `dir/to`.

		Returns `null` if this path does not have a parent element.

		This method does not resolve special names like `.` and `..`.
		That is the parent of `some/..` is `some`.
	**/
	public function parent():Null<FilePath> {
		throw new NotImplementedException();
	}
}