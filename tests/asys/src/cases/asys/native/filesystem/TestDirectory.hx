package cases.asys.native.filesystem;

import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;
import asys.native.filesystem.Directory;

@:depends(cases.asys#if (java && !jvm) ._native #else .native #end.filesystem.TestFileSystem)
class TestDirectory extends FsTest {
	function testOpenNonExistent(async:Async) {
		FileSystem.openDirectory('test-data/temp/non-existent', (e, _) -> {
			assertType(e, FsException, e -> {
				equalPaths('test-data/temp/non-existent', e.path);
				async.done();
			});
		});
	}

	function testNext(async:Async) {
		var expected = ['sub', 'symlink-dir', 'temp', 'bytes.bin', 'symlink'];
		var batchSize = 4;
		var actual = [];
		asyncAll(async,
			FileSystem.openDirectory('test-data', batchSize, (e, dir) -> {
				if(noException(e))
					dir.next((e, r) -> {
						if(noException(e)) {
							equals(batchSize, r.length);
							for(f in r) actual.push(f.toString());
							dir.next((e, r) -> {
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