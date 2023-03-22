package asys.native.filesystem;

import haxe.NoData;
import python.lib.Os;
import python.internal.UBuiltins;
import python.Exceptions.StopIteration;
import asys.native.filesystem.FileSystem.pool;

@:coreApi
class Directory {
	public final path:FilePath;

	final iter:ScandirIterator;
	final maxBatchSize:Int;

	@:allow(asys.native.filesystem)
	function new(iter:ScandirIterator, path:FilePath, maxBatchSize:Int) {
		this.path = path;
		this.iter = iter;
		this.maxBatchSize = maxBatchSize;
	}

	public function next(callback:Callback<Array<FilePath>>):Void {
		pool.runFor(
			() -> {
				var result = [];
				try {
					while(result.length < maxBatchSize)
						result.push(FilePath.ofString(iter.__next__().name));
					result;
				} catch(_:StopIteration) {
					result;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function close(callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(UBuiltins.hasattr(iter, 'close'))
						iter.close();
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}
}