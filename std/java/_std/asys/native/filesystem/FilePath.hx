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

	@:allow(asys.native.filesystem)
	inline function new(path:Path) {
		trace(path.getParent().toString());
		this = path;
	}

	@:from public static inline function fromString(path:String):FilePath {
		return new FilePath(Paths.get(path));
	}

	@:to public function toString():String {
		return switch #if jvm this.toString() #else jObj(this).toString() #end {
			case '': '.';
			case s: s;
		}
	}

	@:op(A == B) function equals(p:FilePath):Bool {
		return #if jvm this.equals(p) #else jObj(this).equals(jObj(this)) #end;
	}

	public inline function isAbsolute():Bool {
		return this.isAbsolute();
	}

	public function absolute():FilePath {
		var fullPath:NativeString = cast #if jvm this.toAbsolutePath().toString() #else jObj(this.toAbsolutePath()).toString() #end;

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
		var path = switch this.getParent() {
			case null if(!this.isAbsolute()):
				Paths.get(Sys.getCwd());
			case path:
				path;
		}
		return new FilePath(path);
	}

#if !jvm
	static inline function jObj(o:Dynamic):java.lang.Object {
		return o;
	}
#end
}