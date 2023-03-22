package asys.native.filesystem;

import haxe.NoData;
import java.nio.file.DirectoryStream;
import java.nio.file.Path;
import java.util.Iterator as JIterator;
import java.util.NoSuchElementException;
import java.lang.Throwable;
import java.nio.file.FileSystemException;
import asys.native.filesystem.FileSystem.pool;

@:coreApi
class Directory {
	public final path:FilePath;

	final stream:DirectoryStream<Path>;
	final iterator:JIterator<Path>;
	final maxBatchSize:Int;

	@:allow(asys.native.filesystem)
	function new(path:FilePath, stream:DirectoryStream<Path>, maxBatchSize:Int) {
		this.path = path;
		this.stream = stream;
		this.maxBatchSize = maxBatchSize;
		this.iterator = stream.iterator();
	}

	public function next(callback:Callback<Array<FilePath>>):Void {
		pool.runFor(
			() -> {
				var result = [];
				try {
					while(result.length < maxBatchSize) {
						result.push((iterator.next().getFileName():FilePath));
					}
					result;
				} catch(_:NoSuchElementException) {
					result;
				} catch(e:Throwable) {
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
					stream.close();
					NoData;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}
}