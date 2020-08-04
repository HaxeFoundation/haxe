package asys.native.filesystem;

import haxe.NoData;
import haxe.EntryPoint;
import haxe.exceptions.NotImplementedException;
import php.Resource;
import php.Global.*;

class Directory {
	public final path:FilePath;
	public var buffer:Int = 32;

	final handle:Resource;

	function new(handle:Resource, path:FilePath) {
		this.handle = handle;
		this.path = path;
	}

	public function next(callback:Callback<Null<FilePath>>) {
		EntryPoint.runInMainThread(() -> {
			var result = try {
				var entry = readdir(handle);
				while(entry != false && (entry == '.' || entry == '..')) {
					entry = readdir(handle);
				}
				entry;
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			switch result {
				case false:
					callback.success(null);
				case (_:String) => s:
					callback.success(s);
			}
		});
	}

	/**
		Close the directory.
	**/
	public function close(callback:Callback<NoData>) {
		EntryPoint.runInMainThread(() -> {
			try {
				closedir(handle);
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(NoData);
		});
	}
}