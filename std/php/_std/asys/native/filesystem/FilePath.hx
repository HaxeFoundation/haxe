package asys.native.filesystem;

import haxe.Callback;
import haxe.io.Bytes;
import haxe.exceptions.NotImplementedException;
import haxe.exceptions.EncodingException;
import php.Global;

@:coreType @:coreApi abstract FilePath {

	@:from public static inline function fromString(path:String):FilePath {
		return cast path;
	}

	@:from public static inline function fromBytes(path:Bytes):FilePath {
		return cast path.toString();
	}

	@:to public inline function toBytes():Bytes {
		return Bytes.ofString(cast this);
	}

	@:to public function toString():String {
		if(!Global.mb_check_encoding(cast this, 'UTF-8'))
			throw new EncodingException('File path is not a valid unicode string');
		return cast this;
	}

	public function toReadableString(patch:Int = '?'.code):String {
		var oldPatch:Any = Global.mb_substitute_character();
		Global.mb_substitute_character(patch);
		var result = Global.mb_scrub(cast this);
		Global.mb_substitute_character(oldPatch);
		return result;
	}

	/**
		Get an absolute path of this path.
		For example translates `./path` to `/current/dir/path`.
	**/
	public function absolute(callback:Callback<Null<FilePath>>):Void {
		callback.fail(new NotImplementedException());
	}

	/**
		Get a canonical path.
		Resolves intermediate `.`, `..` and symbolic links.
		The result may still be a relative path.
	**/
	public function real(callback:Callback<Null<FilePath>>):Void {
		callback.fail(new NotImplementedException());
	}
}