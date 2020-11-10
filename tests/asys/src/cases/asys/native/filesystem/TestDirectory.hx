package cases.asys.native.filesystem;

import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;
import asys.native.filesystem.Directory;

@:depends(cases.asys.native.filesystem.TestFileSystem)
class TestDirectory extends FsTest {
	function testOpenNonExistent(async:Async) {
		FileSystem.openDirectory('test-data/temp/non-existent', (e, _) -> {
			assertType(e, FsException, e -> {
				equals('test-data/temp/non-existent', e.path.toString());
				async.done();
			});
		});
	}

	function testNextEntry(async:Async) {
		var contents = [];
		function read(dir:Directory, callback:()->Void) {
			dir.nextEntry((e, r) -> {
				if(noException(e))
					switch r {
						case null:
							callback();
						case entry:
							contents.push(entry.toString());
							read(dir, callback);

					}
				else
					callback();
			});
		}

		asyncAll(async,
			FileSystem.openDirectory('test-data', (e, dir) -> {
				if(noException(e))
					read(dir, () -> {
						var expected = ['sub', 'symlink-dir', 'temp', 'bytes.bin', 'symlink'];
						expected.sort(Reflect.compare);
						contents.sort(Reflect.compare);
						same(expected, contents);
						dir.close((e, _) -> noException(e));
					});
			})
		);
	}

	function testNextBatch(async:Async) {
		var expected = ['sub', 'symlink-dir', 'temp', 'bytes.bin', 'symlink'];
		var batchSize = 4;
		var actual = [];
		asyncAll(async,
			FileSystem.openDirectory('test-data', (e, dir) -> {
				if(noException(e))
					dir.nextBatch(batchSize, (e, r) -> {
						if(noException(e)) {
							equals(batchSize, r.length);
							for(f in r) actual.push(f.toString());
							dir.nextBatch(batchSize, (e, r) -> {
								if(noException(e)) {
									for(f in r) actual.push(f.toString());
									expected.sort(Reflect.compare);
									actual.sort(Reflect.compare);
									same(expected, actual);
								}
								dir.close((e, _) -> noException(e));
							});
						}
					});
			})
		);
	}
}