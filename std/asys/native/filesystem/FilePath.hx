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
		throw new NotImplementedException();
	}

	/**
		Create a path by sequantually adding `appendices` to `path`.

		If any of the appendices is an absolute path then `path` and all previous
		appendices are discarded.
		Any empty component is ignored.

		TODO: see TODO of "add" method below
	**/
	overload extern static public inline function createPath(path:String, ...appendices:String):FilePath {
		throw new NotImplementedException();
	}

	/**
		Create a path by combining items of `parts` array.

		If any of the `parts` is an absolute path then all previous parts are discarded.
		Any empty component is ignored.

		@throws haxe.exceptions.ArgumentException if `parts` is empty.

		TODO: see TODO of "add" method below
	**/
	overload extern static public inline function createPath(parts:Array<String>):FilePath {
		throw new NotImplementedException();
	}

	/**
		Create file path from plain string.
		Removes trailing slashes.

		Creates a path of `.` if `path` is empty.
		That is `FilePath.ofString('') == FilePath.ofString('.')`.
	**/
	@:noUsing
	@:from static public function ofString(path:String):FilePath {
		throw new NotImplementedException();
	}

	/**
		Alias of `createPath(Array<String>)`
	**/
	@:from static function ofArray(parts:Array<String>):FilePath {
		throw new NotImplementedException();
	}

	/**
		Get string representation of this path.

		Trailing slashes are always removed unless this is a root path.
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

		If this path is already an absolute path, then it's returned as is.

		It does not matter if the path does not exist, however some implementations
		may need current working directory to actually exist.
	**/
	public function absolute():FilePath {
		throw new NotImplementedException();
	}

	/**
		Returns this path with all the redundant elements removed.

		Resolves `.` and `..` and removes excessive slashes and trailing slashes.
		Does not resolve symbolic links.

		This method may return an empty path if all elements of this path are redundant.

		It does not matter if the path does not exist.
	**/
	public function normalize():FilePath {
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

	/**
		Creates a new path by appending `path` to this one.
		This path is treated as a directory and a directory separator is inserted
		between this and `path` if needed.

		If `path` is an absolute path, then this method simply returns `path`.
		If either this or `path` is empty then this method simply return the other one.

		```haxe
		FilePath.ofString('dir').add('file'); // result: dir/file
		FilePath.ofString('dir/').add('file'); // result: dir/file
		FilePath.ofString('dir').add('/file'); // result: /file
		FilePath.ofString('').add('file'); // result: file
		FilePath.ofString('dir').add(''); // result: dir
		```

		TODO:
		What to do with windows paths relative to a drive?
		```haxe
		'D:dir'.add('C:file') == exception ?
		'/dir'.add('C:file') == 'C:/dir/file' ?
		```
	**/
	public function add(path:FilePath):FilePath {
		throw new NotImplementedException();
	}

}