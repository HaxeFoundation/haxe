package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.NativeArray;
import java.NativeString;
import java.io.File as JFile;

private typedef NativeFilePath = Path;

@:coreApi abstract FilePath(NativeFilePath) to NativeFilePath {
	public static var SEPARATOR(get,never):String;
	static inline function get_SEPARATOR():String {
		return JFile.separator;
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
		return new FilePath(Paths.get(path));
	}

	@:from static function ofArray(parts:Array<String>):FilePath {
		if(parts.length == 0)
			throw new ArgumentException('parts');
		var path = ofString(parts[0]);
		for(i in 1...parts.length)
			path = path.add(parts[i]);
		return path;
	}

	@:from static inline function ofNative(path:NativeFilePath):FilePath {
		return new FilePath(path);
	}

	inline function new(path:NativeFilePath) {
		this = path;
	}

	@:to public inline function toString():String {
		return this == null ? null : jObj(this).toString();
	}

	@:op(A == B) inline function equals(p:FilePath):Bool {
		return jObj(this).equals(jObj(this));
	}

	public inline function isAbsolute():Bool {
		return this.isAbsolute();
	}

	public inline function normalize():FilePath {
		return this.normalize();
	}

	public inline function absolute():FilePath {
		return this.toAbsolutePath();
	}

	public inline function parent():Null<FilePath> {
		return this.getParent();
	}

	public function name():FilePath {
		return switch this.getFileName() {
			case null: '';
			case path: path;
		}
	}

	public inline function add(path:FilePath):FilePath {
		return this.resolve(path);
	}

	static inline function jObj(o:NativeFilePath):java.lang.Object {
		return cast o;
	}
}