package cases.asys.native.filesystem;

import haxe.io.Bytes;
import asys.native.filesystem.FilePath;
import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;

class TestFileSystem extends Test {
	function testReadBytes(async:Async) {
		allAsync(async,
			FileSystem.readBytes('test-data/bytes.bin', (e, r) -> {
				if(isNull(e)) {
					var expected = Bytes.alloc(256);
					for(i in 0...expected.length) expected.set(i, i);
					equals(0, r.compare(expected));
				}
			}),
			FileSystem.readBytes('test-data/', (e, r) -> {
				if(isOfType(e, FsException)) {
					equals('test-data/', cast(e, FsException).path);
				}
			}),
			FileSystem.readBytes('non-existent', (e, r) -> {
				if(isOfType(e, FsException)) {
					equals('non-existent', cast(e, FsException).path);
				}
			})
		);
	}

	function testReadString(async:Async) {
		allAsync(async,
			FileSystem.readString('test-data/sub/hello.world', (e, r) -> {
				if(isNull(e)) {
					equals('Hello, world!', r);
				}
			}),
			FileSystem.readString('test-data/', (e, r) -> {
				if(isOfType(e, FsException)) {
					equals('test-data/', cast(e, FsException).path);
				}
			}),
			FileSystem.readString('non-existent', (e, r) -> {
				if(isOfType(e, FsException)) {
					equals('non-existent', cast(e, FsException).path);
				}
			})
		);
	}

	function testIsLink(async:Async) {
		allAsync(async,
			FileSystem.isLink('test-data/symlink', (e, r) -> {
				if(isNull(e)) {
					isTrue(r);
				}
			}),
			FileSystem.isLink('test-data/sub/hello.world', (e, r) -> {
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

	function testReadLink(async:Async) {
		allAsync(async,
			FileSystem.readLink('test-data/symlink', (e, r) -> {
				if(isNull(e)) {
					equals('sub' + FilePath.SEPARATOR + 'hello.world', r);
				}
			}),
			FileSystem.readLink('test-data/sub/hello.world', (e, r) -> {
				if(isOfType(e, FsException)) {
					equals('test-data/sub/hello.world', cast(e, FsException).path);
				}
			}),
			FileSystem.readLink('non-existent', (e, r) -> {
				if(isOfType(e, FsException)) {
					equals('non-existent', cast(e, FsException).path);
				}
			})
		);
	}
}
