package asys.native.filesystem;

import php.*;
import php.Const.*;
import php.Global.*;
import php.Syntax.*;
import php.NativeArray;
import haxe.exceptions.ArgumentException;

private typedef NativeFilePath = NativeString;

@:coreApi abstract FilePath(NativeFilePath) to NativeString {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return DIRECTORY_SEPARATOR;
	}

	overload extern static public inline function createPath(path:String, ...appendices:String):FilePath {
		return createPathImpl(path, ...appendices);
	}

	@:native('createPath')
	static function createPathImpl(path:String, ...appendices:String):FilePath {
		var path = ofString(path);
		for(p in appendices)
			path = path.add(p);
		return path;
	}

	overload extern static public inline function createPath(parts:Array<String>):FilePath {
		return ofArray(parts);
	}

	@:noUsing
	@:from public static inline function ofString(path:String):FilePath {
		return new FilePath(path);
	}

	@:from static function ofArray(parts:Array<String>):FilePath {
		if(parts.length == 0)
			throw new ArgumentException('parts');
		var path = ofString(parts[0]);
		for(i in 1...parts.length)
			path = path.add(parts[i]);
		return path;
	}

	inline function new(s:NativeString) {
		this = s;
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
		return if(this == '') {
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
						var driveCwd = realpath(substr(this, 0, 2) + '.');
						driveCwd + SEPARATOR + substr(this, 2);
					} catch(e:php.Throwable) {
						throw new FsException(CustomError('Unable to get current working directory of drive ${this[0]}'), this);
					}
				}
			} else {
				cwd() + SEPARATOR + this;
			}
		} else {
			cwd() + SEPARATOR + this;
		}
	}

	public function normalize():FilePath {
		var isWin = SEPARATOR == '\\';
		var parts:NativeIndexedArray<String> = if(isWin) {
			(preg_split('#\\\\|/#', this):NativeArray);
		} else {
			explode('/', this);
		}
		var i = count(parts) - 1;
		var result = new NativeIndexedArray();
		var skip = 0;
		while(i >= 0) {
			switch parts[i] {
				case '.' | '':
				case '..':
					++skip;
				case _ if(skip > 0):
					--skip;
				case part:
					array_unshift(result, part);
			}
			--i;
		}
		for(i in 0...skip)
			array_unshift(result, '..');
		var result = ofString(implode(SEPARATOR, result));
		return if(isAbsolute() && !result.isAbsolute()) {
			isSeparator(this[0]) ? SEPARATOR + result : (result == '' ? parts[0] : result) + SEPARATOR;
		} else if(isWin && strlen(this) >= 2 && this[1] == ':' && (strlen(result) < 2 || (result:NativeString)[1] != ':')) {
			(substr(this, 0, 2):String) + (result:NativeString);
		} else {
			result;
		}
	}

	public function parent():Null<FilePath> {
		var path = switch dirname(this) {
			case '.':
				strlen(this) > 1 && this[0] == '.' && isSeparator(this[1]) ? '.' : null;
			case path:
				if(trimSlashes(path) == trimSlashes(this)) {
					null;
				//relative to current drive. E.g. `dirname("C:dir")` returns `C:.`
				} else if(SEPARATOR == '\\' && strlen(path) == 3 && preg_match('/^[a-zA-Z]:\\./', path)) {
					strlen(this) >= 4 && this[2] == '.' && isSeparator(this[3]) ? path : substr(path, 0, 2);
				} else {
					path;
				}
		}
		return new FilePath(path);
	}

	public function add(path:FilePath):FilePath {
		if(path.isAbsolute() || this == '')
			return path;
		if(path == '')
			return this;
		if(SEPARATOR == '\\') {
			var s = (path:NativeString);
			if(strlen(s) >= 2 && s[1] == ':') {
				if(strlen(this) >= 2 && this[1] == ':') {
					if(strtolower(s[0]) != strtolower(this[0])) {
						throw new ArgumentException('path', 'Cannot combine paths on different drives');
					}
					return trimSlashes(this) + SEPARATOR + (substr(s, 2):String);
				} else if(isSeparator(this[0])) {
					return (substr(s, 0, 2):String) + trimSlashes(this) + SEPARATOR + (substr(s, 2):String);
				}
			}
		}
		return trimSlashes(this) + SEPARATOR + path;
	}

	static inline function isSeparator(char:String):Bool {
		return char == '/' || (char == SEPARATOR);
	}

	static inline function trimSlashes(s:NativeString):NativeString {
		return rtrim(s, SEPARATOR == '\\' ? '/\\' : '/');
	}
}