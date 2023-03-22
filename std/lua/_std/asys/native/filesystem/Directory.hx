package asys.native.filesystem;

import haxe.NoData;
import lua.lib.luv.Handle;
import lua.lib.luv.fs.FileSystem as Fs;
import lua.lib.luv.Idle;
import lua.NativeStringTools;
import asys.native.filesystem.FileSystem.xpcall;
import asys.native.filesystem.FileSystem.ioError;

/**
	Represents a directory.
**/
@:coreApi
class Directory {
	/** The path of this directory as it was at the moment of opening the directory */
	public final path:FilePath;

	final dir:Handle;


	function new(dir:Handle, path:FilePath) {
		this.dir = dir;
		this.path = path;
	}

	public function next(callback:Callback<Array<FilePath>>):Void {
		Fs.readdir(dir, xpcall((e,entries) -> switch ioError(e) {
			case null:
				var entries = lua.Table.toArray(entries);
				var result = [for(entry in entries) FilePath.ofString(entry.name)];
				callback.success(result);
			case e:
				callback.fail(new FsException(e, path));
		}));
	}

	public function close(callback:Callback<NoData>):Void {
		Fs.closedir(dir, xpcall((e,_) -> switch ioError(e) {
			case null: callback.success(NoData);
			case e: callback.fail(new FsException(e, path));
		}));
	}
}