package cases.asys.native.filesystem;

import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;
import asys.native.filesystem.Directory;

class TestDirectory extends FsTest {
	function test(async:Async) {
		var contents = [];
		function read(dir:Directory, callback:()->Void) {
			dir.next((e, r) -> {
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
			}),
			FileSystem.openDirectory('test-data/temp/non-existent', (e, _) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non-existent', e.path.toString());
				});
			})
		);
	}
}