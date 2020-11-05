package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import eval.NativeString;

private typedef NativeFilePath = NativeString;

@:coreApi abstract FilePath(NativeFilePath) to NativeString {
	public static var SEPARATOR(get,never):String;
	static var __SEPARATOR:Null<String>;
	static function get_SEPARATOR():String {
		return switch __SEPARATOR {
			case null:
				var s = Sys.systemName() == 'Windows' ? '\\' : '/';
				__SEPARATOR = s;
				s;
			case s:
				(s:String);
		}
	}

	static inline function isSeparator(c:Int):Bool {
		return c == '/'.code || (SEPARATOR == '\\' && c == '\\'.code);
	}

	@:from public static inline function fromString(path:String):FilePath {
		return new FilePath(path);
	}

	inline function new(b:NativeString) {
		this = b;
	}

	@:to public function toString():String {
		var bytes = this.toBytes();
		switch bytes.length {
			case 0 | 1: return bytes.toString();
			//trim trailing slashes
			case (_ - 1) => i:
				while(i > 0 && isSeparator(bytes.get(i))) {
					--i;
				}
				return bytes.getString(0, i + 1);
		}
	}

	@:op(A == B) function equals(p:FilePath):Bool {
		return this == (p:NativeString);
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
			//This is not 100% valid. `Z:some\path` is "a relative path from the current directory of the Z: drive"
			} else if(thisBytes.length > 1 && thisBytes.get(1) == ':'.code) {
				thisBytes;
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

	function join(path:FilePath):FilePath {
		var thisBytes = this.toBytes();
		var i = thisBytes.length - 1;
		var separatorCode = StringTools.fastCodeAt(SEPARATOR, 0);
		while(i >= 0) {
			var c = thisBytes.get(i);
			if(c != separatorCode && c != '/'.code) {
				break;
			}
			--i;
		}
		var buffer = new BytesBuffer();
		buffer.addBytes(thisBytes, 0, (i >= 0 ? i + 1 : thisBytes.length));
		buffer.addByte(separatorCode);
		var thatBytes = (path:NativeString).toBytes();
		buffer.addBytes(thatBytes, 0, thatBytes.length);
		return new FilePath(buffer.getBytes());
	}
}