package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.exceptions.NotImplementedException;

/**
	Represents a relative or absolute file path.

	Most of the time it's a string, but some file systems allow to use arbitrary
	bytes in file names.

	TODO: add API from `haxe.io.Path`
	TODO: `@:coreType` for now as I'm not sure `String` would fit it best for all targets.
**/
@:coreType @:coreApi abstract FilePath {
	public static var SEPARATOR(get,never):String;
	static function get_SEPARATOR():String {
		return Sys.systemName() == 'Windows' ? '\\' : '/';
	}

	/**
		Create file path from plain string.
	**/
	@:from public static function fromString(path:String):FilePath {
		throw new NotImplementedException();
	}

	/**
		Create file path from bytes.
	**/
	@:from public static function fromBytes(path:Bytes):FilePath {
		throw new NotImplementedException();
	}

	/**
		Encode file path to bytes.
	**/
	@:to public function toBytes():Bytes {
		throw new NotImplementedException();
	}

	/**
		Encode file path to string.

		Throws an exception if the path could not be converted to a valid
		unicode string.

		@throws haxe.EncodingException - if current `FilePath` cannot be converted to a unicode string.
	**/
	@:to public function toString():String {
		throw new NotImplementedException();
	}

	@:op(A == B) function equals(p:FilePath):Bool {
		throw new NotImplementedException();
	}

	/**
		Encode file path to a valid unicode string replacing any invalid bytes with `patch`
		unicode character code (the code of `?` is used by default).
	**/
	public function toReadableString(patch:Int = '?'.code):String {
		throw new NotImplementedException();
	}

	/**
		Get an absolute path of this path.
		For example translates `./path` to `/current/dir/path`.
		Does not resolve symbolic links.
	**/
	public function absolute():FilePath {
		throw new NotImplementedException();
	}

	/**
		Get a canonical path.
		Resolves intermediate `.`, `..`, excessive slashes and symbolic links.
		The result may still be a relative path.
	**/
	public function real(callback:Callback<FilePath>):Void {
		throw new NotImplementedException();
	}
}