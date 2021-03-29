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

	@:from public static inline function ofString(path:String):FilePath {
		return new FilePath(path);
	}

	function new(s:NativeString) {
		if(s == null) {
			this = s;
		} else if(s.length == 0) {
			this = '.';
		} else {
			var i = s.length;
			while(i > 1) {
				switch s[i] {
					case '/'.code:
					case '\\'.code if(SEPARATOR == '\\'):
					case _: break;
				}
				--i;
			}
			this = i == s.length ? s : s.sub(1, i);
		}
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

		var parts = if(SEPARATOR == '\\') {
			StringTools.replace(fullPath, '\\', '/').split('/');
		} else {
			fullPath.split('/');
		}
		var i = 1;
		var result = [];
		while(i < parts.length) {
			switch parts[i] {
				case '.' | '':
				case '..':
					result.pop();
				case part:
					result.push(part);
			}
			i++;
		}
		result.unshift(parts[0]);
		return result.join(SEPARATOR);
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

	static inline function isDriveLetter(c:Int):Bool {
		return ('a'.code <= c && c <= 'z'.code) || ('A'.code <= c && c <= 'Z'.code);
	}
}