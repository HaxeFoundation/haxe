package cases;

import sys.FileSystem;
import sys.thread.Mutex;
import sys.thread.Semaphore;

@:timeout(20000)
class TestFileSystem extends ThreadTestBase {
	/** `createDirectory` must not fail when other threads create the same directories at the same time. **/
	function testCreateDirectoryConcurrently() {
		final root = "export/create-directory";
		final numThreads = 8;
		final rounds = 50;
		removeDirectory(root);
		final mutex = new Mutex();
		final errors = [];
		final done = new Semaphore(0);
		for (_ in 0...numThreads) {
			Thread.create(() -> {
				for (round in 0...rounds) {
					try {
						FileSystem.createDirectory('$root/$round/a/b/c');
					} catch (e:Dynamic) {
						mutex.acquire();
						errors.push(Std.string(e));
						mutex.release();
					}
				}
				done.release();
			});
		}
		for (_ in 0...numThreads)
			done.acquire();
		same([], errors);
		for (round in 0...rounds)
			isTrue(FileSystem.isDirectory('$root/$round/a/b/c'));
		removeDirectory(root);
	}

	static function removeDirectory(dir:String) {
		if (!FileSystem.exists(dir))
			return;
		for (entry in FileSystem.readDirectory(dir))
			removeDirectory('$dir/$entry');
		FileSystem.deleteDirectory(dir);
	}
}
