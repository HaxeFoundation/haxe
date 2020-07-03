package asys.native.filesystem;

import haxe.Callback;
import haxe.io.Bytes;
import haxe.exceptions.NotImplementedException;

/**
	Represents a relative or absolute file path.

	Most of the time it's a string, but some file systems allow to use arbitrary
	bytes in file names.

	TODO: add API from `haxe.io.Path`
	TODO: `@:coreType` for now as I'm not sure `String` would fit it best for all targets.
**/
@:coreType abstract FilePath {

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

		TODO: define the exception
	**/
	@:to public function toString():String {
		throw new NotImplementedException();
	}

	/**
		Encode file path to a valid unicode string skipping (replacing?) any invalid bytes

		TODO: decide on skipping/replacing
	**/
	public function toReadableString():String {
		throw new NotImplementedException();
	}

	/**
		Get an absolute path of this path.
		For example translates `./path` to `/current/dir/path`.
	**/
	public function absolute(callback:Callback<Null<FilePath>>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Get a canonical path.
		Resolves intermediate `.`, `..` and symbolic links.
		The result may still be a relative path.
	**/
	public function real(callback:Callback<Null<FilePath>>) {
		callback.fail(new NotImplementedException());
	}
}