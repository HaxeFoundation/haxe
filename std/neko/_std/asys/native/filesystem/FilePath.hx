package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;
import haxe.exceptions.ArgumentException;
import neko.NativeString;

using StringTools;

private typedef NativeFilePath = String;

@:coreApi abstract FilePath(NativeFilePath) {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return _SEPARATOR;
	}

	static var _SEPARATOR:String;

	static function __init__():Void {
		_SEPARATOR = neko.Lib.load("std","sys_string",0)() == NativeString.ofString('Windows') ? '\\' : '/';
	}

	overload extern static public inline function createPath(path:String, ...appendices:String):FilePath {
		return createPathImpl(path, ...appendices);
	}

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

	@:to inline function toNativeString():NativeString {
		return NativeString.ofString(this);
	}

	inline function new(s:String) {
		this = s;
	}

	@:to public inline function toString():String {
		return this;
	}

	public function isAbsolute():Bool {
		if(this == '')
			return false;
		if(this.fastCodeAt(0) == '/'.code)
			return true;
		if(SEPARATOR == '\\') {
			return switch this.fastCodeAt(0) {
				case '\\'.code:
					true;
				case c if(isDriveLetter(c)):
					this.length > 1 && this.fastCodeAt(1) == ':'.code;
				case _:
					false;
			}
		}
		return false;
	}

	public function absolute():FilePath {
		var result = if(this == '') {
			Sys.getCwd();
		} else if(this.fastCodeAt(0) == '/'.code) {
			this;
		} else if(SEPARATOR == '\\') {
			if(this.fastCodeAt(0) == '\\'.code) {
				this;
			} else if(this.length >= 2 && isDriveLetter(this.fastCodeAt(0)) && this.fastCodeAt(1) == ':'.code) {
				if(this.length > 2 && isSeparator(this.fastCodeAt(2))) {
					this;
				} else {
					try {
						var driveCwd = @:privateAccess FileSystem.file_full_path(NativeString.ofString(this.charAt(0) + ':.'));
						NativeString.toString(driveCwd) + SEPARATOR + this.substr(2);
					} catch(_) {
						throw new FsException(CustomError('Unable to get current working directory of drive ${this.charAt(0)]}'), this);
					}
				}
			} else {
				Sys.getCwd() + this;
			}
		} else {
			Sys.getCwd() + this;
		}
		return result;
	}

	public function normalize():FilePath {
		var parts = if(SEPARATOR == '\\') {
			this.replace('\\', '/').split('/');
		} else {
			this.split('/');
		}
		var i = parts.length - 1;
		var result = [];
		var skip = 0;
		while(i >= 0) {
			switch parts[i] {
				case '.' | '':
				case '..':
					++skip;
				case _ if(skip > 0):
					--skip;
				case part:
					result.unshift(part);
			}
			--i;
		}
		for(i in 0...skip)
			result.unshift('..');
		var result = ofString(result.join(SEPARATOR));
		return isAbsolute() && !result.isAbsolute() ? SEPARATOR + result : result;
	}

	public function parent():Null<FilePath> {
		var s = trimSlashes(this);
		var i = s.length;
		var isWin = SEPARATOR == '\\';
		while(i >= 0) {
			switch s.fastCodeAt(i) {
				case '/'.code: break;
				case '\\'.code if(isWin): break;
				case _:
			}
			--i;
		}
		//no directory in this path
		return if(i < 0) {
			null;
		//this == '/' or this == '\'
		} else if(i == 0 && s.length == 1) {
			null;
		} else {
			new FilePath(s.substr(0, i));
		}
	}

	public function add(path:FilePath):FilePath {
		if(path.isAbsolute() || this == '')
			return path;
		if(path == '')
			return this;
		if(SEPARATOR == '\\') {
			var s = path.toString();
			if(s.length >= 2 && s.fastCodeAt(1) == ':'.code) {
				if(this.length >= 2 && this.fastCodeAt(1) == ':'.code) {
					if(s.substr(0, 1).toLowerCase() != this.substr(0, 1).toLowerCase()) {
						throw new ArgumentException('path', 'Cannot combine paths on different drives');
					}
					return trimSlashes(this) + SEPARATOR + s.substr(2);
				} else if(isSeparator(this.fastCodeAt(0))) {
					return s.substr(0, 2) + trimSlashes(this) + SEPARATOR + s.substr(2);
				}
			}
		}
		return trimSlashes(this) + SEPARATOR + path;
	}

	static inline function isDriveLetter(c:Int):Bool {
		return ('a'.code <= c && c <= 'z'.code) || ('A'.code <= c && c <= 'Z'.code);
	}

	/**
	 * Trims all trailing slashes even if it's the last slash of a root path.
	 */
	static function trimSlashes(s:String):String {
		var i = s.length - 1;
		while(i >= 0) {
			switch s.fastCodeAt(i) {
				case '/'.code:
				case '\\'.code if(SEPARATOR == '\\'):
				case _: break;
			}
			--i;
		}
		return i == s.length - 1 ? s : s.substr(0, i + 1);
	}

	static inline function isSeparator(char:Int):Bool {
		return char == '/'.code || (SEPARATOR == '\\' && char == '\\'.code);
	}
}