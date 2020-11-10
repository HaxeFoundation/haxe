package asys.native.filesystem;

import haxe.NoData;
import haxe.EntryPoint;
import haxe.IJobExecutor;
import haxe.exceptions.NotImplementedException;
import php.Resource;
import php.Global.*;

class Directory {
	public final path:FilePath;

	final handle:Resource;
	final executor:IJobExecutor;

	function new(handle:Resource, path:FilePath, executor:IJobExecutor) {
		this.handle = handle;
		this.path = path;
		this.executor = executor;
	}

	public function nextEntry(callback:Callback<Null<FilePath>>) {
		executor.addJob(
			() -> {
				var result = try {
					var entry = readdir(handle);
					while(entry != false && (entry == '.' || entry == '..')) {
						entry = readdir(handle);
					}
					entry;
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
				switch result {
					case false: null;
					case (_:String) => s: (s:FilePath);
				}
			},
			callback
		);
	}

	public function nextBatch(maxBatchSize:Int, callback:Callback<Array<FilePath>>) {
		executor.addJob(
			() -> {
				try {
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
					throw new FsException(CustomError(e.getMessage()), path);
				}
			},
			callback
		);
	}

	public function close(callback:Callback<NoData>) {
		executor.addJob(
			() -> {
				try {
					closedir(handle);
				} catch(e:php.Exception) {
					throw new FsException(CustomError(e.getMessage()), path);
				}
				NoData;
			},
			callback
		);
	}
}