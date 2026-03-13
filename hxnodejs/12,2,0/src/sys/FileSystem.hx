package sys;

import js.node.Fs;
import js.node.Path;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

@:dce
@:coreApi
class FileSystem {
	public static function exists(path:String):Bool {
		return try {
			Fs.accessSync(path);
			true;
		} catch (_:Dynamic) false;
	}

	public static inline function rename(path:String, newPath:String):Void {
		Fs.renameSync(path, newPath);
	}

	public static inline function stat(path:String):sys.FileStat {
		return cast Fs.statSync(path);
	}

	public static inline function fullPath(relPath:String):String {
		return try Fs.realpathSync(relPath) catch (e:Dynamic) null;
	}

	public static inline function absolutePath(relPath:String):String {
		if (haxe.io.Path.isAbsolute(relPath))
			return relPath;
		return js.node.Path.resolve(relPath);
	}

	public static function isDirectory(path:String):Bool {
		return try Fs.statSync(path).isDirectory() catch (e:Dynamic) false;
	}

	public static function createDirectory(path:String):Void {
		try {
			Fs.mkdirSync(path);
		} catch (e:Dynamic) {
			if (e.code == "ENOENT") {
				// parent doesn't exist - create parent and then this dir
				createDirectory(Path.dirname(path));
				Fs.mkdirSync(path);
			} else {
				// some other error - check if path is a dir, rethrow the error if not
				// (the `(e : Error)` cast is here to avoid HaxeError wrapping, though we need to investigate this in Haxe itself)
				var stat = try Fs.statSync(path) catch (_:Dynamic) throw(e : Error);
				if (!stat.isDirectory())
					throw(e : Error);
			}
		}
	}

	public static inline function deleteFile(path:String):Void {
		Fs.unlinkSync(path);
	}

	public static inline function deleteDirectory(path:String):Void {
		if (exists(path)) {
			for (file in readDirectory(path)) {
				var curPath = path + "/" + file;
				if (isDirectory(curPath)) {
					deleteDirectory(curPath);
				} else {
					deleteFile(curPath);
				}
			}
			js.node.Fs.rmdirSync(path);
		}
	}

	public static inline function readDirectory(path:String):Array<String> {
		return Fs.readdirSync(path);
	}
}
