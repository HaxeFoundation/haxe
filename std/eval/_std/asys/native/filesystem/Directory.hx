package asys.native.filesystem;

import haxe.NoData;
import eval.luv.Dir;
import sys.thread.Thread;

@:coreApi
class Directory {
	public final path:FilePath;

	final dir:Dir;

	function new(dir:Dir, path:FilePath) {
		this.dir = dir;
		this.path = path;
	}

	public function nextEntry(callback:Callback<Null<FilePath>>):Void {
		dir.read(Thread.current().events, 1, null, r -> switch r {
			case Error(e):
				callback.fail(new FsException(e, path));
			case Ok(entries):
				switch entries.length {
					case 0: callback.success(null);
					case 1: callback.success(@:privateAccess new FilePath(entries[0].name));
					case _: callback.fail(new FsException(CustomError('Unexpected direcotry entries amount read'), path));
				}
		});
	}

	public function nextBatch(maxBatchSize:Int, callback:Callback<Array<FilePath>>):Void {
		dir.read(Thread.current().events, maxBatchSize, null, r -> switch r {
			case Error(e):
				callback.fail(new FsException(e, path));
			case Ok(entries):
				var result = [for(e in entries) @:privateAccess new FilePath(e.name)];
				callback.success(result);
		});
	}

	public function close(callback:Callback<NoData>):Void {
		dir.close(Thread.current().events, null, r -> switch r {
			case Error(e): callback.fail(new FsException(e, path));
			case Ok(_): callback.success(NoData);
		});
	}
}