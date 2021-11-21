package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import cs.system.io.Path;
import cs.NativeArray;

using StringTools;

private typedef NativeFilePath = String;
private typedef NativeString = cs.system.String;

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
		var result = parts[0];
		for(i in 1...parts.length) {
			result = Path.Combine(result, parts[i]);
		}
		return new FilePath(result);
	}

	@:from static inline function ofNative(path:NativeFilePath):FilePath {
		return new FilePath(path);
	}

	inline function new(path:NativeFilePath) {
		this = path;
	}

	public inline function add(path:FilePath):FilePath {
		return Path.Combine(this, path);
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
		return Path.GetFullPath(this == '' ? '.' : this);
	}

	public function parent():Null<FilePath> {
		return switch Path.GetDirectoryName(this == '' ? '.' : trimSlashes(this)) {
			case '': null;
			case path: new FilePath(path);
		}
	}

	public function name():FilePath {
		return Path.GetFileName(trimSlashes(this));
	}

	public function normalize():FilePath {
		var delimiter = if(SEPARATOR == '\\') {
			var str = new NativeArray<String>(2);
			str[0] = '\\';
			str[1] = '/';
			str;
		} else {
			var str = new NativeArray<String>(1);
			str[0] = '/';
			str;
		}
		var parts = (cast this:NativeString).Split(delimiter, cs.system.StringSplitOptions.None);
		var i = parts.Length - 1;
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
}