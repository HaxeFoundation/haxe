package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import lua.lib.luv.fs.FileSystem as LuvFs;

using lua.NativeStringTools;

private abstract NativeString(String) from String to String {
	public var length(get,never):Int;
	inline function get_length():Int
		return NativeStringTools.len(this);

	@:op([]) inline function get(i:Int)
		return NativeStringTools.byte(this, i);

	public inline function sub(start:Int, ?end:Int)
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

	@:from static function ofArray(parts:Array<String>):FilePath {
		if(parts.length == 0)
			throw new ArgumentException('parts');
		var path = ofString(parts[0]);
		for(i in 1...parts.length)
			path = path.add(parts[i]);
		return path;
	}

	overload extern static public inline function createPath(path:String, ...appendices:String):FilePath {
		return createPathImpl(path, ...appendices);
	}

	overload extern static public inline function createPath(parts:Array<String>):FilePath {
		return ofArray(parts);
	}

	@:native('createPath')
	static function createPathImpl(path:String, ...appendices:String):FilePath {
		var path = ofString(path);
		for(p in appendices)
			path = path.add(p);
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
		var result = if(this == '') {
			Sys.getCwd();
		} else if(this[1] == '/'.code) {
			this;
		} else if(SEPARATOR == '\\') {
			if(this[1] == '\\'.code) {
				this;
			} else if(this.length >= 2 && isDriveLetter(this[1]) && this[2] == ':'.code) {
				if(this.length > 2 && isSeparator(this[3])) {
					this;
				} else {
					try {
						var driveCwd = LuvFs.realpath(this.sub(1, 2) + '.');
						driveCwd + SEPARATOR + this.sub(3);
					} catch(_) {
						throw new FsException(CustomError('Unable to get current working directory of drive ${this.sub(1,1)]}'), new FilePath(this));
					}
				}
			} else {
				Sys.getCwd() + SEPARATOR + this;
			}
		} else {
			Sys.getCwd() + SEPARATOR + this;
		}
		return new FilePath(result);
	}

	public function normalize():FilePath {
		var parts = if(SEPARATOR == '\\') {
			StringTools.replace(this, '\\', '/').split('/');
		} else {
			(this:String).split('/');
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
		while(i >= 1) {
			switch s[i] {
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
		} else if(i == 1 && s.length == 1) {
			null;
		} else {
			new FilePath(s.sub(1, i));
		}
	}

	public function name():Null<FilePath> {
		var s = trimSlashes(this);
		var i = s.length;
		var isWin = SEPARATOR == '\\';
		while(i >= 1) {
			if(s[i] == '/'.code || (isWin && s[i] == '\\'.code)) {
				break;
			}
			--i;
		}
		//no directory in this path
		return new FilePath(i < 1 ? s : s.sub(i + 1));
	}

	public function add(path:FilePath):FilePath {
		if(path.isAbsolute() || this == '')
			return path;
		if(path == '')
			return new FilePath(this);
		if(SEPARATOR == '\\') {
			var s = (cast path:NativeString);
			if(s.length >= 2 && s[2] == ':'.code) {
				if(this.length >= 2 && this[2] == ':'.code) {
					if(s.sub(1,1).lower() != this.sub(1,1).lower()) {
						throw new ArgumentException('path', 'Cannot combine paths on different drives');
					}
					return new FilePath(trimSlashes(this) + SEPARATOR + s.sub(3));
				} else if(isSeparator(this[1])) {
					return new FilePath(s.sub(1, 2) + trimSlashes(this) + SEPARATOR + s.sub(3));
				}
			}
		}
		return new FilePath(trimSlashes(this) + SEPARATOR + path);
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

	static inline function isSeparator(char:Int):Bool {
		return char == '/'.code || (SEPARATOR == '\\' && char == '\\'.code);
	}
}