package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.EntryPoint;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import php.Global.*;
import php.Syntax;
import php.NativeArray;
import php.NativeIndexedArray;
import php.Resource;

/**
	File system operations.
**/
@:coreApi
class FileSystem {

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		EntryPoint.runInMainThread(() -> {
			var file = try {
				@:privateAccess new File(fopenHx(cast path, flag), path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(cast file);
		});
	}

	static public function tempFile(callback:Callback<File>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				tmpfile();
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), '(unknown path)'));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to create a temporary file'), '(unknown path)'));
				case _:
					callback.success(@:privateAccess new File(result, stream_get_meta_data(result)['uri']));
			}
		});
	}

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				file_get_contents(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false: callback.fail(new FsException(CustomError('Failed to read a file'), path));
				case r: callback.success(Bytes.ofString(r));
			}
		});
	}

	static public function readString(path:FilePath, callback:Callback<String>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				file_get_contents(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false: callback.fail(new FsException(CustomError('Failed to read a file'), path));
				case r: callback.success(r);
			}
		});
	}

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var f = fopenHx(cast path, flag);
				fwrite(f, data.getData().toString());
				fclose(f);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var f = fopenHx(cast path, flag);
				fwrite(f, text);
				fclose(f);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				opendir(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to open a directory'), path));
				case _:
					callback.success(@:privateAccess new Directory(result, path));
			}
		});
	}

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				scandir(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to list a directory'), path));
				case (_:NativeIndexedArray<String>) => list:
					callback.success([for(item in list) if(item != '.' && item != '..') (item:FilePath)]);
			}
		});
	}

	static public function createDirectory(path:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				mkdir(cast path, permissions, recursive);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to create a directory'), path));
			}
		});
	}

	static public function uniqueDirectory(prefix:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void {
		EntryPoint.runInMainThread(() -> {
			var dirPath = try {
				var path:String = (cast prefix:String) + getRandomChar() + getRandomChar() + getRandomChar() + getRandomChar();
				while(true) {
					try {
						if(mkdir(path, permissions, recursive)) {
							break;
						} else {
							throw new php.Exception('Failed to create a directory');
						}
					} catch(e:php.Exception) {
						switch strpos(e.getMessage(), 'mkdir(): File exists') {
							case false:
								throw e;
							case _:
								path += getRandomChar();
						}
					}
				}
				path;
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), prefix));
				return;
			}
			callback.success(dirPath);
		});
	}

	static var __codes:Null<Array<String>>;
	static function getRandomChar():String {
		//TODO: null safety issue if `switch` result is assigned directly to this var declaration
		var codes:Array<String>;
		switch __codes {
			case null:
				var a = [for(c in '0'.code...'9'.code) String.fromCharCode(c)];
				for(c in 'A'.code...'Z'.code) a.push(String.fromCharCode(c));
				for(c in 'a'.code...'z'.code) a.push(String.fromCharCode(c));
				codes = __codes = a;
			case a:
				codes = a;
		}
		return codes[Std.random(codes.length)];
	}

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			if(!overwrite && file_exists(cast newPath)) {
				callback.fail(new FsException(FileExists, newPath));
				return;
			}
			var success = try {
				moveRecursive(cast oldPath, cast newPath);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), oldPath));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to move file or directory'), oldPath));
			}
		});
	}

	/**
	 * This is required to avoid "Directory not empty" warning from `rename` function
	 */
	static function moveRecursive(oldPath:String, newPath:String):Bool {
		if(is_dir(newPath) && is_dir(oldPath)) {
			var dir = opendir(oldPath);
			var success = true;
			if(dir == false)
				throw new FsException(CustomError('Failed to read directory'), oldPath);
			try {
				while(true) {
					switch readdir(dir) {
						case '.' | '..':
						case false:
							break;
						case entry:
							success = moveRecursive('$oldPath/$entry', '$newPath/$entry') && success;
					}
				}
			} catch(e:php.Exception) {
				try closedir(dir) catch(_) {}
				throw e;
			}
			return success;
		} else {
			return rename(oldPath, newPath);
		}
	}

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				unlink(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to delete a file'), path));
			}
		});
	}

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				rmdir(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to delete a file'), path));
			}
		});
	}

	static public function info(path:FilePath, callback:Callback<FileInfo>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				stat(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to stat'), path));
				case _:
					callback.success(phpStatToHx(result));
			}
		});
	}

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
					(!mode.has(Exists) || file_exists(cast path))
					&& (!mode.has(Readable) || is_readable(cast path))
					&& (!mode.has(Writable) || is_writable(cast path))
					&& (!mode.has(Executable) || is_executable(cast path));
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				is_dir(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				is_file(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				chmod(cast path, permissions);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success)
				callback.success(NoData)
			else
				callback.fail(new FsException(CustomError('Failed to set permissions'), path));
		});
	}

	static public function setOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				chown(cast path, userId) && chgrp(cast path, groupId);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success)
				callback.success(NoData)
			else
				callback.fail(new FsException(CustomError('Failed to set owner'), path));
		});
	}

	static public function setLinkOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var success = try {
				lchown(cast path, userId) && lchgrp(cast path, groupId);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success)
				callback.success(NoData)
			else
				callback.fail(new FsException(CustomError('Failed to set owner'), path));
		});
	}

	static public function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		var path:FilePath = path == null ? basename(cast target) : path;
		EntryPoint.runInMainThread(() -> {
			var success = try {
				switch type {
					case SymLink: symlink(cast target, cast path);
					case HardLink: php.Global.link(cast target, cast path);
				}
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			if(success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to create a link'), path));
			}
		});
	}

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				is_link(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				readlink(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false: callback.fail(new FsException(CustomError('Failed to read a link'), path));
				case r: callback.success(r);
			}
		});
	}

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				lstat(cast path);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to stat'), path));
				case _:
					callback.success(phpStatToHx(result));
			}
		});
	}

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			if(!overwrite && file_exists(cast destination)) {
				callback.fail(new FsException(FileExists, destination));
				return;
			}
			var success = try {
				copy(cast source, cast destination);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), source));
				return;
			}
			if (success) {
				callback.success(NoData);
			} else {
				callback.fail(new FsException(CustomError('Failed to copy a file'), source));
			}
		});
	}

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var f = fopen(cast path, 'r+');
				var result = ftruncate(f, newSize);
				fclose(f);
				result;
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to resize file'), path));
				case _:
					callback.success(NoData);
			}
		});
	}

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				touch(cast path, modificationTime, accessTime);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.fail(new FsException(CustomError('Failed to set file times'), path));
				case _:
					callback.success(NoData);
			}
		});
	}

	static function fopenHx(file:String, flag:FileOpenFlag<Dynamic>):Resource {
		var f = switch flag {
			case Append: fopen(file, 'a');
			case AppendRead: fopen(file, 'a+');
			case Read: fopen(file, 'r');
			case ReadWrite: fopen(file, 'r+');
			case Write: fopen(file, 'w');
			case WriteX: fopen(file, 'x');
			case WriteRead: fopen(file, 'w+');
			case WriteReadX: fopen(file, 'x+');
			case Overwrite: fopen(file, 'c');
			case OverwriteRead: fopen(file, 'c+');
		}
		if(f == false)
			throw new php.Exception('Cannot open file');
		return f;
	}

	static function phpStatToHx(phpStat:NativeArray):FileInfo {
		return {
			atime: phpStat['atime'],
			mtime: phpStat['mtime'],
			ctime: phpStat['ctime'],
			dev: phpStat['dev'],
			gid: phpStat['gid'],
			uid: phpStat['uid'],
			ino: phpStat['ino'],
			mode: phpStat['mode'],
			nlink: phpStat['nlink'],
			rdev: phpStat['rdev'],
			size: phpStat['size'],
			blksize: phpStat['blksize'],
			blocks: phpStat['blocks']
		}
	}
}