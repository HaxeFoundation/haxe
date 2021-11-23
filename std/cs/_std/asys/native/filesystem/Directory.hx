package asys.native.filesystem;

import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import cs.system.Exception as CsException;
import asys.native.filesystem.FileSystem.pool;
import asys.native.filesystem.FileSystem.rethrow;
import cs.system.collections.IEnumerator;

@:coreApi
class Directory {
	public final path:FilePath;
	final maxBatchSize:Int;

#if (net_ver >= 40)
	final contents:IEnumerator;

	@:allow(asys.native.filesystem)
	function new(contents:IEnumerator, path:FilePath, maxBatchSize:Int) {
		this.contents = contents;
		this.maxBatchSize = maxBatchSize;
		this.path = path;
	}

	public function next(callback:Callback<Array<FilePath>>):Void {
		pool.runFor(
			() -> {
				try {
					var result = [];
					for(i in 0...maxBatchSize) {
						if(!contents.MoveNext())
							break;
						result.push((contents.Current:FilePath));
					}
					result;
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}
#else
	final contents:Array<FilePath>;
	var current = 0;

	@:allow(asys.native.filesystem)
	function new(contents:Array<FilePath>, path:FilePath, maxBatchSize:Int) {
		this.contents = contents;
		this.maxBatchSize = maxBatchSize;
		this.path = path;
	}

	public function next(callback:Callback<Array<FilePath>>):Void {
		pool.runFor(
			() -> {
				try {
					if(current < contents.length - 1) {
						var result = contents.slice(current, maxBatchSize);
						current += maxBatchSize;
						result;
					} else {
						[];
					}
				} catch(e:CsException) {
					rethrow(e, path);
				}
			},
			callback
		);
	}
#end
	public function close(callback:Callback<NoData>):Void {
		pool.runFor(() -> NoData, callback);
	}
}