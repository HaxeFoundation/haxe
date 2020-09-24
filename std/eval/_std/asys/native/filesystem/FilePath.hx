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
				s;
		}
	}

	@:from public static function fromString(path:String):FilePath {
		return new FilePath(Bytes.ofString(path));
	}

	@:to public function toString():String {
		return this.toString();
	}

	@:op(A == B) inline function equals(p:FilePath):Bool {
		return this.compare(p.asBytes()) == 0;
	}

	public function absolute():FilePath {
		inline function withCwd() {
			var b = new BytesBuffer();
			b.addString(Sys.getCwd());
			b.addBytes(this, 0, this.length);
			return b.getBytes();
		}
		var fullPath = if(this.length == 0) {
			withCwd();
		} else if(this.get(0) == '/'.code) {
			this;
		} else if(SEPARATOR == '\\') {
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

		var parts = [];
		for(i in 0...fullPath.length) {
			switch fullPath.get(i) {
				case '.'.code:
				case '/'.code:
				case '\\'.code if(SEPARATOR == '\\'):
				case c:
					result.addByte(c);
			}
		}
		array_unshift(result, parts[0]);
		return implode(SEPARATOR, result);
	}

	inline function asBytes():Bytes {
		return this;
	}
}