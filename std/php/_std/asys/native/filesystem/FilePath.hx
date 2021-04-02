package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.EntryPoint;
import php.*;
import php.Const.*;
import php.Global.*;
import php.Syntax.*;
import php.NativeArray;

private typedef NativeFilePath = NativeString;

@:coreApi abstract FilePath(NativeFilePath) to NativeString {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return DIRECTORY_SEPARATOR;
	}

	@:allow(asys.native.filesystem)
	function new(s:NativeString) {
		this = switch s {
			case null: null;
			case '': '.';
			case _:
				if(SEPARATOR == '\\') {
					var trimmed:NativeString = rtrim(s, '\\/');
					var length = strlen(s);
					var lengthTrimmed = strlen(trimmed);
					//current disk root. E.g. `\`
					if(trimmed == '') {
						SEPARATOR;
					//specific disk root. E.g. `C:\`
					} else if(lengthTrimmed == 2 && trimmed[1] == ':' && length >= 3 && isSeparator(s[2])) {
						trimmed + SEPARATOR;
					} else {
						trimmed;
					}
				} else {
					switch rtrim(s, SEPARATOR) {
						case '': s[0];
						case s: s;
					}
				}
		}
	}

	@:from public static inline function ofString(path:String):FilePath {
		return new FilePath(path);
	}

	@:to public inline function toString():String {
		return this;
	}

	public function isAbsolute():Bool {
		if(this == '')
			return false;
		if(this[0] == '/')
			return true;
		if(SEPARATOR == '\\') {
			if(this[0] == '\\')
				return true;
			if(preg_match('#^[a-zA-Z]:(\\\\|/)#', this))
				return true;
		}
		return false;
	}

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
			} else if(preg_match('/^[a-zA-Z]:/', this)) {
				if(strlen(this) > 2 && isSeparator(this[2])) {
					this;
				} else {
					try {
						var driveCwd = realpath(substr(this, 0, 2));
						driveCwd + SEPARATOR + substr(this, 2);
					} catch(e:php.Throwable) {
						throw new FsException(CustomError('Unable to get current working directory of drive ${this[0]}'), this);
					}
				}
			} else {
				rtrim(cwd() + SEPARATOR + this, '\\/');
			}
		} else {
			rtrim(cwd() + SEPARATOR + this, '/');
		}

		var parts:NativeIndexedArray<String> = if(SEPARATOR == '\\') {
			(preg_split('#\\\\|/#', fullPath):NativeArray);
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

	public function parent():Null<FilePath> {
		var path = switch dirname(this) {
			case '.':
				strlen(this) > 1 && this[0] == '.' && isSeparator(this[1]) ? '.' : null;
			case path:
				path == this ? null : path;
		}
		return new FilePath(path);
	}

	static inline function isSeparator(char:String):Bool {
		return char == '/' || (char == SEPARATOR);
	}
}