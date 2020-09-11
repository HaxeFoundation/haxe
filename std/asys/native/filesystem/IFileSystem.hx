package asys.native.filesystem;

import haxe.io.Bytes;
import haxe.NoData;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;

@:inheritDoc(asys.native.filesystem.FileSystem)
interface IFileSystem {

	@:inheritDoc(asys.native.filesystem.FileSystem.openFile)
	public function openFile<T>(path:FilePath, flag:FileOpenFlag<T>, callback:Callback<T>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.tempFile)
	public function tempFile(callback:Callback<File>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.readBytes)
	public function readBytes(path:FilePath, callback:Callback<Bytes>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.readString)
	public function readString(path:FilePath, callback:Callback<String>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.writeBytes)
	public function writeBytes(path:FilePath, data:Bytes, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.writeString)
	public function writeString(path:FilePath, text:String, flag:FileOpenFlag<Dynamic> = Write, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.openDirectory)
	public function openDirectory(path:FilePath, callback:Callback<Directory>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.listDirectory)
	public function listDirectory(path:FilePath, callback:Callback<Array<FilePath>>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.createDirectory)
	public function createDirectory(path:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.uniqueDirectory)
	public function uniqueDirectory(prefix:FilePath, ?permissions:FilePermissions, recursive:Bool = false, callback:Callback<FilePath>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.move)
	public function move(oldPath:FilePath, newPath:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.deleteFile)
	public function deleteFile(path:FilePath, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.deleteDirectory)
	public function deleteDirectory(path:FilePath, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.info)
	public function info(path:FilePath, callback:Callback<FileInfo>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.check)
	public function check(path:FilePath, mode:FileAccessMode, callback:Callback<Bool>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.isDirectory)
	public function isDirectory(path:FilePath, callback:Callback<Bool>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.isFile)
	public function isFile(path:FilePath, callback:Callback<Bool>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.setPermissions)
	public function setPermissions(path:FilePath, permissions:FilePermissions, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.setOwner)
	public function setOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.setLinkOwner)
	public function setLinkOwner(path:FilePath, user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.link)
	public function link(target:FilePath, ?path:FilePath, type:FileLink = SymLink, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.isLink)
	public function isLink(path:FilePath, callback:Callback<Bool>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.readLink)
	public function readLink(path:FilePath, callback:Callback<FilePath>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.linkInfo)
	public function linkInfo(path:FilePath, callback:Callback<FileInfo>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.copyFile)
	public function copyFile(source:FilePath, destination:FilePath, overwrite:Bool = true, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.resize)
	public function resize(path:FilePath, newSize:Int, callback:Callback<NoData>):Void;

	@:inheritDoc(asys.native.filesystem.FileSystem.setTimes)
	public function setTimes(path:FilePath, accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void;
}