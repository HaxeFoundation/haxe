package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.exceptions.NotImplementedException;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.NativeArray;
import java.NativeString;
import java.io.File as JFile;

private typedef NativeFilePath = Path;

@:coreApi abstract FilePath(NativeFilePath) {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return JFile.separator;
	}

	inline function new(path:Path) {
		this = path;
	}

	@:allow(asys.native.filesystem)
	inline function javaPath():Path {
		return this;
	}

	@:from public static inline function fromString(path:String):FilePath {
		return new FilePath(Paths.get(path, new NativeArray(0)));
	}

	@:from public static function fromBytes(path:Bytes):FilePath {
		return new FilePath(Paths.get(path.getString(0, path.length, RawNative), new NativeArray(0)));
	}

	@:to public inline function toBytes():Bytes {
		return Bytes.ofString(this.toString());
	}

	@:to public inline function toString():String {
		return this.toString();
	}

	@:op(A == B) function equals(p:FilePath):Bool {
		return this.equals(p.javaPath());
	}

	public function absolute():FilePath {
		return new FilePath(this.toAbsolutePath());
	}
}