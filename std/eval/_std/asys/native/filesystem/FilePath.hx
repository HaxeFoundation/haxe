package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.io.BytesBuffer;

private typedef NativeFilePath = Bytes;

@:coreApi abstract FilePath(NativeFilePath) {
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

	@:from public static function fromString(path:String):FilePath {
		return new FilePath(Bytes.ofString(path));
	}

	inline function new(b:Bytes) {
		this = b;
	}

	@:to public function toString():String {
		switch this.length {
			case 0 | 1: return this.toString();
			//trim trailing slashes
			case (_ - 1) => i:
				while(i > 0 && isSeparator(this.get(i))) {
					--i;
				}
				return this.getString(0, i + 1);
		}
	}

	@:op(A == B) inline function equals(p:FilePath):Bool {
		return this.compare(p.asBytes()) == 0;
	}

	public function absolute():FilePath {
		var separatorCode = StringTools.fastCodeAt(SEPARATOR, 0);
		inline function withCwd() {
			var b = new BytesBuffer();
			b.addString(Sys.getCwd());
			b.addByte(separatorCode);
			b.addBytes(this, 0, this.length);
			return b.getBytes();
		}
		var fullPath = if(this.length == 0) {
			withCwd();
		} else if(this.get(0) == '/'.code) {
			this;
		} else if(separatorCode == '\\'.code) {
			if(this.get(0) == '\\'.code) {
				this;
			//This is not 100% valid. `Z:some\path` is "a relative path from the current directory of the Z: drive"
			} else if(this.length > 1 && this.get(1) == ':'.code) {
				this;
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

	inline function asBytes():Bytes {
		return this;
	}
}