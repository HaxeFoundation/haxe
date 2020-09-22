package asys.native.filesystem;

import haxe.NoData;
import haxe.IJobExecutor;
import java.nio.file.DirectoryStream;
import java.nio.file.Path;
import java.util.Iterator as JIterator;
import java.util.NoSuchElementException;
import java.lang.Throwable;
import java.nio.file.FileSystemException;

@:coreApi
class Directory {
	public final path:FilePath;

	final stream:DirectoryStream<Path>;
	final iterator:JIterator<Path>;
	final jobs:IJobExecutor;

	public var buffer:Int = 32;

	@:allow(asys.native.filesystem)
	function new(path:FilePath, stream:DirectoryStream<Path>, jobs:IJobExecutor) {
		this.path = path;
		this.stream = stream;
		this.iterator = stream.iterator();
		this.jobs = jobs;
	}

	public function next(callback:Callback<Null<FilePath>>):Void {
		jobs.addJob(
			() -> {
				try {
					new FilePath(iterator.next().getFileName());
				} catch(_:NoSuchElementException) {
					null;
				} catch(e:FileSystemException) {
					throw new FsException(CustomError(e.getReason()), path);
				} catch(e:Throwable) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function close(callback:Callback<NoData>):Void {
		jobs.addJob(
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