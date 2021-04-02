package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;
import cs.system.io.Path;

using StringTools;

private typedef NativeFilePath = String;

@:coreApi abstract FilePath(NativeFilePath) {
	public static var SEPARATOR(get,never):String;
	@:pure(true) static inline function get_SEPARATOR():String {
		return untyped __cs__('{0}.ToString()', Path.DirectorySeparatorChar);
	}

	static inline function isSeparator(c:Int):Bool {
		return c == '/'.code || (SEPARATOR == '\\' && c == '\\'.code);
	}

	static function trimSlashes(s:String):String {
		var i = s.length - 1;
		if(i <= 0)
			return s;
		var sep = isSeparator(s.fastCodeAt(i));
		if(sep) {
			do {
				--i;
				sep = isSeparator(s.fastCodeAt(i));
			} while(i > 0 && sep);
			return s.substr(0, i + 1);
		} else {
			return s;
		}
	}

	@:from public static inline function ofString(path:String):FilePath {
		return new FilePath(path);
	}

	function new(s:NativeFilePath) {
		this = switch s {
			case null: null;
			case _ if(s.length == 0): '.';
			case _:
				var trimmed = trimSlashes(s);
				switch trimmed.length {
					case 0: s.charAt(0);
					case 2 if(SEPARATOR == '\\' && s.fastCodeAt(1) == ':'.code && isSeparator(s.fastCodeAt(2))): s.substr(0, 3);
					case _: trimmed;
				}
		}
	}

	@:to public inline function toString():String {
		return this;
	}

	public function isAbsolute():Bool {
		if(Path.IsPathRooted(this)) {
			if(SEPARATOR == '\\' && this.length >= 2 && this.fastCodeAt(1) == ':'.code) {
				return isSeparator(this.fastCodeAt(2));
			} else {
				return true;
			}
		} else {
			return false;
		}
	}

	public function absolute():FilePath {
		return Path.GetFullPath(this);
	}

	public function parent():Null<FilePath> {
		return switch Path.GetDirectoryName(this) {
			case '': null;
			case path: new FilePath(path);
		}
	}
}