package asys;

import haxe.Error;
import haxe.io.Bytes;
import haxe.io.FilePath;
import asys.io.FileReadStream;

typedef FileReadStreamCreationOptions = {
	?flags:FileOpenFlags,
	?mode:FilePermissions
} &
	asys.io.FileReadStream.FileReadStreamOptions;

class FileSystem {
	public static inline final async = asys.AsyncFileSystem;

	extern public static function access(path:FilePath, ?mode:FileAccessMode = FileAccessMode.Ok):Void;

	extern public static function chmod(path:FilePath, mode:FilePermissions, ?followSymLinks:Bool = true):Void;

	extern public static function chown(path:FilePath, uid:Int, gid:Int, ?followSymLinks:Bool = true):Void;

	public static function copyFile(src:FilePath, dest:FilePath /* , ?flags:FileCopyFlags */):Void {
		throw "not implemented";
	}

	public static function createReadStream(path:FilePath, ?options:FileReadStreamCreationOptions):FileReadStream {
		if (options == null)
			options = {};
		return new FileReadStream(open(path, options.flags, options.mode), options);
	}

	// static function createWriteStream(path:FilePath, ?options:{?flags:FileOpenFlags, ?mode:FilePermissions, ?autoClose:Bool, ?start:Int}):FileWriteStream;

	extern public static function exists(path:FilePath):Bool;

	extern public static function link(existingPath:FilePath, newPath:FilePath):Void;

	extern static function mkdir_native(path:FilePath, mode:FilePermissions):Void;

	public static function mkdir(path:FilePath, ?recursive:Bool = false, ?mode:FilePermissions):Void {
		if (mode == null)
			mode = @:privateAccess new FilePermissions(511); // 0777
		if (!recursive)
			return mkdir_native(path, mode);
		var pathBuffer:FilePath = null;
		for (component in path.components) {
			if (pathBuffer == null)
				pathBuffer = component;
			else
				pathBuffer = pathBuffer / component;
			try {
				mkdir_native(pathBuffer, mode);
			} catch (e:Error) {
				if (e.type.match(UVError(asys.uv.UVErrorType.EEXIST)))
					continue;
				throw e;
			}
		}
	}

	extern public static function mkdtemp(prefix:FilePath):FilePath;

	public static function readdir(path:FilePath):Array<FilePath> {
		return readdirTypes(path).map(entry -> entry.name);
	}

	extern public static function readdirTypes(path:FilePath):Array<DirectoryEntry>;

	extern public static function readlink(path:FilePath):FilePath;

	extern public static function realpath(path:FilePath):FilePath;

	extern public static function rename(oldPath:FilePath, newPath:FilePath):Void;

	extern public static function rmdir(path:FilePath):Void;

	extern public static function stat(path:FilePath, ?followSymLinks:Bool = true):eval.uv.Stat;

	extern public static function symlink(target:FilePath, path:FilePath, ?type:SymlinkType = SymlinkType.SymlinkDir):Void;

	public static function truncate(path:FilePath, ?len:Int = 0):Void {
		var f = open(path, FileOpenFlags.ReadWrite);
		try {
			f.truncate(len);
		} catch (e:Dynamic) {
			f.close();
			throw e;
		}
		f.close();
	}

	extern public static function unlink(path:FilePath):Void;

	extern static function utimes_native(path:FilePath, atime:Float, mtime:Float):Void;

	public static function utimes(path:FilePath, atime:Date, mtime:Date):Void {
		utimes_native(path, atime.getTime(), mtime.getTime());
	}

	public static inline function watch(path:FilePath, ?recursive:Bool = false):FileWatcher {
		return @:privateAccess new FileWatcher(path, recursive);
	}

	extern static function open_native(path:FilePath, flags:FileOpenFlags, mode:FilePermissions, binary:Bool):asys.io.File;

	public static function open(path:FilePath, ?flags:FileOpenFlags = FileOpenFlags.ReadOnly, ?mode:FilePermissions, ?binary:Bool = true):asys.io.File {
		if (mode == null)
			mode = @:privateAccess new FilePermissions(438); // 0666
		return open_native(path, flags, mode, binary);
	}

	public static function readFile(path:FilePath, ?flags:FileOpenFlags = FileOpenFlags.ReadOnly):Bytes {
		var file = open(path, flags);
		var buffer:haxe.io.Bytes;
		try {
			var size = file.stat().size;
			buffer = Bytes.alloc(size);
			file.readBuffer(buffer, 0, size, 0);
		} catch (e:Dynamic) {
			file.close();
			throw e;
		}
		file.close();
		return buffer;
	}

	@:access(asys.FileOpenFlags)
	public static function writeFile(path:FilePath, data:Bytes, ?flags:FileOpenFlags, ?mode:FilePermissions):Void {
		if (flags == null)
			flags = "w";
		if (mode == null)
			mode = @:privateAccess new FilePermissions(438) /* 0666 */;
		var file = open(path, flags, mode);
		var offset = 0;
		var length = data.length;
		var position:Null<Int> = null;
		if (flags.get_raw() & FileOpenFlags.Append.get_raw() == 0)
			position = 0;
		try {
			while (length > 0) {
				var written = file.writeBuffer(data, offset, length, position).bytesWritten;
				offset += written;
				length -= written;
				if (position != null) {
					position += written;
				}
			}
		} catch (e:Dynamic) {
			file.close();
			throw e;
		}
	}
}
