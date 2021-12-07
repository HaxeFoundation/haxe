package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import hl.uv.FileSync;

using StringTools;

private typedef NativeFilePath = String;

@:coreApi abstract FilePath(NativeFilePath) to String {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return _SEPARATOR;
	}

	static var _SEPARATOR:String;

	static function __init__():Void {
		_SEPARATOR = Sys.systemName() == 'Windows' ? '\\' : '/';
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

	inline function new(s:String) {
		this = s;
	}

	@:to public inline function toString():String {
		return this == null ? null : this.toString();
	}

	public function isAbsolute():Bool {
		return switch this.length {
			case 0: false;
			case _ if(isSeparator(this.fastCodeAt(0))): true;
			case 1: false;
			case length if(SEPARATOR == '\\'): this.fastCodeAt(1) == ':'.code && length >= 3 && isSeparator(this.fastCodeAt(2));
			case _: false;
		}
	}

	public function parent():Null<FilePath> {
		var s = trimSlashes(this);
		switch s.length {
			case 0:
				return null;
			case 1 if(isSeparator(s.fastCodeAt(0))):
				return null;
			case 2 | 3 if(SEPARATOR == '\\' && s.fastCodeAt(1) == ':'.code):
				return null;
			case (_ - 1) => i:
				while(!isSeparator(s.fastCodeAt(i))) {
					--i;
					if(i < 0)
						return null;
				}
				return new FilePath(s.substr(0, i + 1));
		}
	}

	public function name():FilePath {
		var s = trimSlashes(this);
		var i = s.length - 1;
		if(i < 0)
			return s;
		while(!isSeparator(s.fastCodeAt(i))) {
			--i;
			if(i < 0)
				return s;
		}
		return new FilePath(s.substr(i + 1));
	}

	public function absolute():FilePath {
		var result = if(this.length == 0) {
			trimSlashes(Sys.getCwd());
		} else if(this.fastCodeAt(0) == '/'.code) {
			this;
		} else if(SEPARATOR == '\\') {
			if(this.fastCodeAt(0) == '\\'.code) {
				this;
			} else if(this.length >= 2 && isDriveLetter(this.fastCodeAt(0)) && this.fastCodeAt(1) == ':'.code) {
				if(this.length > 2 && isSeparator(this.fastCodeAt(3))) {
					this;
				} else {
					try {
						var driveCwd = FileSync.realPath(this.substr(0, 2) + '.');
						driveCwd + SEPARATOR + this.substr(2);
					} catch(_) {
						throw new FsException(CustomError('Unable to get current working directory of drive ${this.substr(0,1)]}'), new FilePath(this));
					}
				}
			} else {
				trimSlashes(Sys.getCwd()) + SEPARATOR + this;
			}
		} else {
			trimSlashes(Sys.getCwd()) + SEPARATOR + this;
		}
		return new FilePath(result);
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

	public function add(path:FilePath):FilePath {
		if(path.isAbsolute() || this.length == 0)
			return path;
		var strPath = (path:String);
		if(strPath.length == 0)
			return new FilePath(this);
		if(SEPARATOR == '\\') {
			if(strPath.length >= 2 && strPath.fastCodeAt(1) == ':'.code) {
				if(this.length >= 2 && this.fastCodeAt(1) == ':'.code) {
					if(strPath.charAt(0).toLowerCase() != this.charAt(0).toLowerCase()) {
						throw new ArgumentException('path', 'Cannot combine paths on different drives');
					}
					return new FilePath(trimSlashes(this) + SEPARATOR + strPath.substr(2));
				} else if(isSeparator(this.fastCodeAt(0))) {
					return new FilePath(strPath.substr(0, 2) + trimSlashes(this) + SEPARATOR + strPath.substr(2));
				}
			}
		}
		return new FilePath(trimSlashes(this) + SEPARATOR + strPath);
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

	static inline function isDriveLetter(c:Int):Bool {
		return ('a'.code <= c && c <= 'z'.code) || ('A'.code <= c && c <= 'Z'.code);
	}
}