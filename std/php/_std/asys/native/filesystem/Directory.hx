package asys.native.filesystem;

import haxe.NoData;
import php.Resource;
import php.Global.*;

class Directory {
	public final path:FilePath;

	final handle:Resource;

	static inline function run(job:()->Void):Void {
		inline haxe.EntryPoint.runInMainThread(job);
	}

	function new(handle:Resource, path:FilePath) {
		this.handle = handle;
		this.path = path;
	}

	public function nextEntry(callback:Callback<Null<FilePath>>) {
		run(() -> {
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
				case false: callback.success(null);
				case (_:String) => s: callback.success(s);
			}
		});
	}

	public function nextBatch(maxBatchSize:Int, callback:Callback<Array<FilePath>>) {
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