package asys.native.filesystem;

import haxe.NoData;
import php.Resource;
import php.Global.*;

class Directory {
	public final path:FilePath;

	final handle:Resource;
	final maxBatchSize:Int;

	static inline function run(job:()->Void):Void {
		inline haxe.EntryPoint.runInMainThread(job);
	}

	function new(handle:Resource, path:FilePath, maxBatchSize:Int) {
		this.handle = handle;
		this.path = path;
		this.maxBatchSize = maxBatchSize;
	}

	public function next(callback:Callback<Array<FilePath>>) {
		run(() -> {
			var result = try {
				var entries = [];
				while(entries.length < maxBatchSize) {
					var entry = readdir(handle);
					while(entry != false && (entry == '.' || entry == '..')) {
						entry = readdir(handle);
					}
					if(entry != false) {
						entries.push(@:privateAccess new FilePath(entry));
					} else {
						break;
					}
				}
				entries;
			} catch(e:php.Exception) {
				callback.fail(new FsException(CustomError(e.getMessage()), path));
				return;
			}
			callback.success(result);
		});
	}

	public function close(callback:Callback<NoData>) {
		run(() -> {
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