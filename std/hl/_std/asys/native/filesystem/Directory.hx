package asys.native.filesystem;

import haxe.NoData;
import hl.uv.UVError;
import hl.uv.Dir;
import asys.native.filesystem.FileSystem.currentLoop;
import asys.native.filesystem.FileSystem.ioError;

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
		dir.read(currentLoop(), maxBatchSize, (e, entries) -> switch e {
			case UV_NOERR:
				var result = [for(e in entries) @:privateAccess new FilePath(e.name)];
				callback.success(result);
			case _:
				callback.fail(new FsException(ioError(e), path));
		});
	}

	public function close(callback:Callback<NoData>):Void {
		dir.close(currentLoop(), e -> switch e {
			case UV_NOERR: callback.success(NoData);
			case _: callback.fail(new FsException(ioError(e), path));
		});
	}
}