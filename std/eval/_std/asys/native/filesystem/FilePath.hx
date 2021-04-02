package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import eval.NativeString;

private typedef NativeFilePath = NativeString;

@:coreApi abstract FilePath(NativeFilePath) to NativeString {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return _SEPARATOR;
	}

	static var _SEPARATOR:String;

	static function __init__():Void {
		_SEPARATOR = Sys.systemName() == 'Windows' ? '\\' : '/';
	}

	static inline function isSeparator(c:Int):Bool {
		return c == '/'.code || (SEPARATOR == '\\' && c == '\\'.code);
	}

	static function trimSlashes(s:NativeString):NativeString {
		var i = s.length - 1;
		if(i <= 0)
			return s;
		var sep = isSeparator(s.code(i));
		if(sep) {
			do {
				--i;
				sep = isSeparator(s.code(i));
			} while(i > 0 && sep);
			return s.sub(0, i + 1);
		} else {
			return s;
		}
	}

	@:from public static inline function ofString(path:String):FilePath {
		return new FilePath(path);
	}

	function new(s:NativeString) {
		this = switch s {
			case null: null;
			case _ if(s.length == 0): '.';
			case _:
				var trimmed = trimSlashes(s);
				switch trimmed.length {
					case 0: SEPARATOR;
					case 2 if(SEPARATOR == '\\' && s.code(1) == ':'.code && isSeparator(s.code(2))): s.sub(0, 3);
					case _: trimmed;
				}
		}
	}

	@:to public function toString():String {
		return this.toString();
	}

	@:op(A == B) inline function equals(p:FilePath):Bool {
		return this == (p:NativeString);
	}

	public function isAbsolute():Bool {
		return switch this.length {
			case 0: false;
			case _ if(isSeparator(this.code(0))): true;
			case 1: false;
			case length if(SEPARATOR == '\\'): this.code(1) == ':'.code && length >= 3 && isSeparator(this.code(2));
			case _: false;
		}
	}

	public function parent():Null<FilePath> {
		switch this.length {
			case 0:
				return null;
			case 1 if(isSeparator(this.code(0))):
				return null;
			case 2 if(SEPARATOR == '\\' && this.code(1) == ':'.code):
				return null;
			case (_ - 1) => i:
				while(!isSeparator(this.code(i))) {
					--i;
					if(i < 0)
						return null;
				}
				return new FilePath(this.sub(0, i + 1));

		}
	}

	public function absolute():FilePath {
		var thisBytes = this.toBytes();
		var separatorCode = StringTools.fastCodeAt(SEPARATOR, 0);
		inline function withCwd() {
			var b = new BytesBuffer();
			b.addString(Sys.getCwd());
			b.addByte(separatorCode);
			b.addBytes(thisBytes, 0, thisBytes.length);
			return b.getBytes();
		}
		var fullPath = if(thisBytes.length == 0) {
			withCwd();
		} else if(thisBytes.get(0) == '/'.code) {
			thisBytes;
		} else if(separatorCode == '\\'.code) {
			if(thisBytes.get(0) == '\\'.code) {
				thisBytes;
			//Starts with `C:`
			} else if(thisBytes.length > 1 && thisBytes.get(1) == ':'.code) {
				//absolute path with a drive. E.g. `C:/some/path`
				if(thisBytes.length > 2 && isSeparator(thisBytes.get(2))) {
					thisBytes;
				//relative to specified drive. E.g. `C:some/path`
				} else {
					var driveCwd = NativeString.fromString(sys.FileSystem.fullPath(this.sub(0, 2).toString()));
					if(thisBytes.length > 2) {
						(driveCwd + NativeString.fromString(SEPARATOR) + this.sub(2)).toBytes();
					} else {
						driveCwd.toBytes();
					}
				}
			} else {
				withCwd();
			}
		} else {
			withCwd();
		}

		var dots = 0;
		var slash = true;
		var skip = 0;
		var lastIndex = fullPath.length - 1;
		var i = lastIndex;
		var slashIndex = fullPath.length;
		var parts = [];
		while(i >= 0) {
			switch fullPath.get(i) {
				case '.'.code if(slash):
					++dots;
				case c:
					// found a slash
					if(c == separatorCode || c == '/'.code) {
						// already have slash and only dots in between
						if(slash) {
							switch dots {
								//multiple slashes or `/./`
								case 0 | 1:
								// `/../`
								case 2:
									++skip;
								// other amounts of dots may be a regular file name
								case _:
									if(skip > 0) --skip
									else parts.unshift(fullPath.sub(i + 1, slashIndex - (i + 1)));
							}
						} else {
							//ignore trailing slash
							if(i == lastIndex) {
							//if currently skipping after a `..`
							} else if(skip > 0) {
								--skip;
							} else {
								parts.unshift(fullPath.sub(i + 1, slashIndex - (i + 1)));
							}
							slash = true;
						}
						slashIndex = i;
					// not a slash and not a dot and not skipping current part
					} else {
						slash = false;
					}
					dots = 0;
			}
			--i;
		}
		if(slashIndex > 0) {
			parts.unshift(fullPath.sub(0, slashIndex));
		}

		var result = new BytesBuffer();

		if(parts.length > 0) {
			if(separatorCode == '\\'.code) {
				var b = parts[0];
				if(b.length < 2 || b.get(1) != ':'.code) {
					result.addByte(separatorCode);
				}
			} else {
				result.addByte(separatorCode);
			}
			for(i => b in parts) {
				result.addBytes(b, 0, b.length);
				if(i < parts.length - 1)
					result.addByte(separatorCode);
			}
		}

		return new FilePath(result.getBytes());
	}
}