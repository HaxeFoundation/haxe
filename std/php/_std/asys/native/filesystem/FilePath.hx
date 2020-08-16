package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.EntryPoint;
import haxe.exceptions.EncodingException;
import php.*;
import php.Global.*;
import php.Syntax.*;
import php.NativeArray;

private typedef NativeFilePath = php.NativeString;

@:coreApi abstract FilePath(NativeFilePath) {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return php.Const.DIRECTORY_SEPARATOR;
	}

	@:allow(asys.native.filesystem)
	inline function new(s:String) {
		this = s;
	}

	@:allow(asys.native.filesystem)
	inline function phpStr():php.NativeString {
		return this;
	}

	@:from public static inline function fromString(path:String):FilePath {
		return new FilePath(path);
	}

	@:from public static inline function fromBytes(path:Bytes):FilePath {
		return new FilePath(path.toString());
	}

	@:to public inline function toBytes():Bytes {
		return Bytes.ofString(this);
	}

	@:to public function toString():String {
		if(!mb_check_encoding(this, 'UTF-8'))
			throw new EncodingException('File path is not a valid unicode string');
		return this;
	}

	public function toReadableString(patch:Int = '?'.code):String {
		var oldPatch:Any = mb_substitute_character();
		mb_substitute_character(patch);
		var result = mb_scrub(this);
		mb_substitute_character(oldPatch);
		return result;
	}

	/**
		Get an absolute path of this path.
		For example translates `./path` to `/current/dir/path`.
	**/
	public function absolute():FilePath {
		inline function cwd():String {
			var result = getcwd();
			if(result == false)
				throw new FsException(CustomError('Unable to get current working directory'), this);
			return result;
		}
		var fullPath = if(this == '') {
			cwd();
		} else if(this[0] == '/') {
			this;
		} else if(SEPARATOR == '\\') {
			if(this[0] == '\\') {
				this;
			//This is not 100% valid. `Z:some\path` is "a relative path from the current directory of the Z: drive"
			//but PHP doesn't have a function to get current directory of another drive
			} else if(preg_match('/^[a-zA-Z]:/', this)) {
				this;
			} else {
				rtrim(cwd() + SEPARATOR + this, '\\/');
			}
		} else {
			rtrim(cwd() + SEPARATOR + this, '/');
		}

		var parts:NativeIndexedArray<String> = if(SEPARATOR == '\\') {
			(preg_split('#\\|/#', fullPath):NativeArray);
		} else {
			explode('/', fullPath);
		}
		var i = 1;
		var result = new NativeIndexedArray();
		while(i < count(parts)) {
			switch parts[i] {
				case '.' | '':
				case '..':
					array_pop(result);
				case part:
					result.push(part);
			}
			i++;
		}
		array_unshift(result, parts[0]);
		return implode(SEPARATOR, result);
	}

	public function real(callback:Callback<FilePath>):Void {
		EntryPoint.runInMainThread(() -> {
			var resolved = realpath(this);
			if(resolved == false) {
				callback.fail(new FsException(CustomError('Unable to resolve real path'), new FilePath(this)));
			} else {
				callback.success((resolved:String));
			}
		});
	}
}