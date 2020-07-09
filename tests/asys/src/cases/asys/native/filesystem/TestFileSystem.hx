package cases.asys.native.filesystem;

import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;

class TestFileSystem extends Test {
	function testIsLink(async:Async) {
		allAsync(async,
			FileSystem.isLink('test-data/symlink', (e, r) -> {
				if(isNull(e)) {
					isTrue(r);
				}
			}),
			FileSystem.isLink('test-data/sub/empty.file', (e, r) -> {
				if(isNull(e)) {
					isFalse(r);
				}
			}),
			FileSystem.isLink('test-data', (e, r) -> {
				if(isNull(e)) {
					isFalse(r);
				}
			}),
			FileSystem.isLink('non-existent', (e, r) -> {
				if(isNull(e)) {
					isFalse(r);
				}
			})
		);
	}
}
