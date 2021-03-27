package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.exceptions.NotImplementedException;
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

	static final empty = Paths.get('');

	@:allow(asys.native.filesystem)
	inline function new(path:Path) {
		this = jObj(empty).equals(path) ? Paths.get('.') : path;
	}

	@:from public static inline function ofString(path:String):FilePath {
		return new FilePath(Paths.get(path));
	}

	@:to public inline function toString():String {
		return jObj(this).toString();
	}

	@:op(A == B) inline function equals(p:FilePath):Bool {
		return jObj(this).equals(jObj(this));
	}

	public inline function isAbsolute():Bool {
		return this.isAbsolute();
	}

	public function absolute():FilePath {
		var abs = this.toAbsolutePath();
		var fullPath:NativeString = cast jObj(abs).toString();

		var parts:NativeArray<String> = if(SEPARATOR == '\\') {
			fullPath.split('\\|/');
		} else {
			fullPath.split('/');
		}

		var i = 1;
		var result = new NativeArray(parts.length);
		result[0] = parts[0];
		var resultSize = 1;
		while(i < parts.length) {
			switch parts[i] {
				case '.' | '':
				case '..':
					if(resultSize > 1) --resultSize;
				case part:
					result[resultSize++] = part;
			}
			i++;
		}

		var builder = new java.lang.StringBuilder();
		for(i in 0...resultSize) {
			if(i != 0)
				builder.append(SEPARATOR);
			builder.append(result[i]);
		}
		return new FilePath(Paths.get(builder.toString()));
	}

	public function parent():Null<FilePath> {
		return switch this.getParent() {
			case null: null;
			case path: new FilePath(path);
		}
	}

	static inline function jObj(o:Path):java.lang.Object {
		return cast o;
	}
}