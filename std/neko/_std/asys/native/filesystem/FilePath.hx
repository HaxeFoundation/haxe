package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;
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

	@:from public static inline function fromString(path:String):FilePath {
		return new FilePath(path);
	}

	@:allow(asys.native.filesystem)
	function new(s:String) {
		if(s == null || s.length == 1) {
			this = s;
		} else if(s.length == 0) {
			this = '.';
		} else {
			var i = s.length - 1;
			while(i > 0) {
				switch s.fastCodeAt(i) {
					case '/'.code:
					case '\\'.code if(SEPARATOR == '\\'):
					case _: break;
				}
				--i;
			}
			this = if(i + 1 == s.length) {
				s;
			} else {
				s.substr(0, i + 1);
			}
		}
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
		var fullPath = if(isAbsolute()) {
			this;
		} else {
			Sys.getCwd() + '/' + this;
		}

		var parts = if(SEPARATOR == '\\') {
			fullPath.replace('\\', '/').split('/');
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
		var i = this.length - 1;
		var isWin = SEPARATOR == '\\';
		while(i >= 0) {
			switch this.fastCodeAt(i) {
				case '/'.code: break;
				case '\\'.code if(isWin): break;
				case _:
			}
			--i;
		}
		//no directory in this path
		return if(i < 0) {
			switch this {
				case '..':
					new FilePath(Sys.getCwd()).parent();
				case _:
					new FilePath(Sys.getCwd());
			}
		//this == '/' or this == '\'
		} else if(i == 0) {
			switch this {
				case '/': null;
				case '\\' if(isWin): null;
				case _: new FilePath(this.charAt(0));
			}
		//this = 'C:\'
		} else if(i == 2 && isWin && this.fastCodeAt(1) == ':'.code && isDriveLetter(this.fastCodeAt(0))) {
			return null;
		} else {
			new FilePath(this.substr(0, i + 1));
		}
	}

	static inline function isDriveLetter(c:Int):Bool {
		return ('a'.code <= c && c <= 'z'.code) || ('A'.code <= c && c <= 'Z'.code);
	}
}