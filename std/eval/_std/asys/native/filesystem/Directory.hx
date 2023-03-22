package asys.native.filesystem;

import haxe.NoData;
import eval.luv.Dir;
import sys.thread.Thread;

@:coreApi
class Directory {
	public final path:FilePath;

	final dir:Dir;
	final maxBatchSize:Int;

	function new(dir:Dir, path:FilePath, maxBatchSize:Int) {
		this.dir = dir;
		this.path = path;
		this.maxBatchSize = maxBatchSize;
	}

	public function next(callback:Callback<Array<FilePath>>):Void {
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