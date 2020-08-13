package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.EntryPoint;
import haxe.NoData;
import haxe.IJobExecutor;

/**
	File system operations.
**/
@:coreApi
class FileSystem {
	static public function create(executor:IJobExecutor = null):IFileSystem {
		return new FileSystemImpl(executor == null ? Native.defaultExecutor : executor);
	}

	static public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void
		new FileSystemImpl(Native.defaultExecutor).openFile(path, flag, callback);

	static public function tempFile(callback:Callback<File>):Void
		new FileSystemImpl(Native.defaultExecutor).tempFile(callback);

	static public function readBytes(path:FilePath, callback:Callback<Bytes>):Void
		new FileSystemImpl(Native.defaultExecutor).readBytes(path, callback);

	static public function readString(path:FilePath, callback:Callback<String>):Void
		new FileSystemImpl(Native.defaultExecutor).readString(path, callback);

	static public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).writeBytes(path, data, flag, callback);

	static public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).writeString(path, text, flag, callback);

	static public function openDirectory(path:FilePath, callback:Callback<Directory>):Void
		new FileSystemImpl(Native.defaultExecutor).openDirectory(path, callback);

	static public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void
		new FileSystemImpl(Native.defaultExecutor).listDirectory(path, callback);

	static public function createDirectory(path:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).createDirectory(path, permissions, recursive, callback);

	static public function uniqueDirectory(prefix:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void
		new FileSystemImpl(Native.defaultExecutor).uniqueDirectory(prefix, permissions, recursive, callback);

	static public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).move(oldPath, newPath, overwrite, callback);

	static public function deleteFile(path:FilePath, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).deleteFile(path, callback);

	static public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).deleteDirectory(path, callback);

	static public function info(path:FilePath, callback:Callback<FileInfo>):Void
		new FileSystemImpl(Native.defaultExecutor).info(path, callback);

	static public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void
		new FileSystemImpl(Native.defaultExecutor).check(path, mode, callback);

	static public function isDirectory(path:FilePath, callback:Callback<Bool>):Void
		new FileSystemImpl(Native.defaultExecutor).isDirectory(path, callback);

	static public function isFile(path:FilePath, callback:Callback<Bool>):Void
		new FileSystemImpl(Native.defaultExecutor).isFile(path, callback);

	static public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setPermissions(path, permissions, callback);

	static public function setOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setOwner(path, userId, groupId, callback);

	static public function setLinkOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setLinkOwner(path, userId, groupId, callback);

	static public function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).link(target, path, type, callback);

	static public function isLink(path:FilePath, callback:Callback<Bool>):Void
		new FileSystemImpl(Native.defaultExecutor).isLink(path, callback);

	static public function readLink(path:FilePath, callback:Callback<FilePath>):Void
		new FileSystemImpl(Native.defaultExecutor).readLink(path, callback);

	static public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void
		new FileSystemImpl(Native.defaultExecutor).linkInfo(path, callback);

	static public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).copyFile(source, destination, overwrite, callback);

	static public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).resize(path, newSize, callback);

	static public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void
		new FileSystemImpl(Native.defaultExecutor).setTimes(path, accessTime, modificationTime, callback);
}


private class FileSystemImpl implements IFileSystem {
	final jobs:IJobExecutor;

	public inline function new(jobs:IJobExecutor) {
		this.jobs = jobs;
	}

	public inline function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void {
		throw new haxe.exceptions.NotImplementedException();
		// jobs.addJob(
		// 	() -> {
		// 	},
		// 	callback
		// );
	}

	public inline function tempFile(callback:Callback<File>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function readBytes(path:FilePath, callback:Callback<Bytes>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function readString(path:FilePath, callback:Callback<String>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function openDirectory(path:FilePath, callback:Callback<Directory>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function createDirectory(path:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function uniqueDirectory(prefix:FilePath, permissions:FilePermissions = 511, recursive:Bool = false, callback:Callback<FilePath>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function deleteFile(path:FilePath, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function info(path:FilePath, callback:Callback<FileInfo>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function isDirectory(path:FilePath, callback:Callback<Bool>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function isFile(path:FilePath, callback:Callback<Bool>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function setOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function setLinkOwner(path:FilePath, userId:Int, groupId:Int, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function isLink(path:FilePath, callback:Callback<Bool>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function readLink(path:FilePath, callback:Callback<FilePath>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public inline function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		throw new haxe.exceptions.NotImplementedException();
	}
}