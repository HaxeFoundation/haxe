package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;

using lua.NativeStringTools;

private abstract NativeString(String) from String to String {
	public var length(get,never):Int;
	inline function get_length():Int
		return NativeStringTools.len(this);

	@:op([]) inline function get(i:Int)
		return NativeStringTools.byte(this, i);

	public inline function sub(start:Int, end:Int)
		return NativeStringTools.sub(this, start, end).match;
}

private typedef NativeFilePath = NativeString;

@:coreApi abstract FilePath(NativeFilePath) {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return _SEPARATOR;
	}

	static var _SEPARATOR:String;

	static function __init__():Void {
		_SEPARATOR = Sys.systemName() == 'Windows' ? '\\' : '/';
	}

	@:noUsing
	@:from public static inline function ofString(path:String):FilePath {
		return new FilePath(path);
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
		if(this[1] == '/'.code)
			return true;
		if(SEPARATOR == '\\') {
			return switch this[1] {
				case '\\'.code:
					true;
				case c if(isDriveLetter(c)):
					this.length > 1 && this[2] == ':'.code;
				case _:
					false;
			}
		}
		return false;
	}

	public function absolute():FilePath {
		var fullPath:String = isAbsolute() ? this : Sys.getCwd() + '/' + this;
	}

	public function normalize():FilePath {
		var parts = if(SEPARATOR == '\\') {
			StringTools.replace(fullPath, '\\', '/').split('/');
		} else {
			fullPath.split('/');
		}
		var i = parts.length - 1;
		var result = [];
		var skip = 0;
		while(i >= 1) {
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
		var i = this.length;
		var isWin = SEPARATOR == '\\';
		while(i >= 1) {
			switch this[i] {
				case '/'.code: break;
				case '\\'.code if(isWin): break;
				case _:
			}
			--i;
		}
		//no directory in this path
		return if(i < 1) {
			null;
		//this == '/' or this == '\'
		} else if(i == 1 && this.length == 1) {
			return null;
		} else {
			new FilePath(this.sub(1, i));
		}
	}

	public function add(path:FilePath):FilePath {
		if(path.isAbsolute() || this == '')
			return path;
		if(path == '')
			return this;
		if(SEPARATOR == '\\') {
			var s = (cast path:NativeString);
			if(s.length >= 2 && s[1] == ':') {
				if(this.length >= 2 && this[1] == ':') {
					if(s[0].sub(1,1).lower() != this[0].sub(1,1).lower()) {
						throw new ArgumentException('path', 'Cannot combine paths on different drives');
					}
					return trimSlashes(this) + SEPARATOR + s.sub(3);
				} else if(isSeparator(this[0])) {
					return s.sub(1, 2) + trimSlashes(this) + SEPARATOR + s.sub(3);
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
	static function trimSlashes(s:NativeString):NativeString {
		var i = s.length;
		while(i >= 1) {
			switch s[i] {
				case '/'.code:
				case '\\'.code if(SEPARATOR == '\\'):
				case _: break;
			}
			--i;
		}
		return i == s.length ? s : s.sub(1, i);
	}
}