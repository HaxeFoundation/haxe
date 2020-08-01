package cases.asys.native.filesystem;

import haxe.Int64;
import haxe.Callback;
import haxe.io.Bytes;
import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;
import asys.native.filesystem.File;

@:depends(cases.asys.native.filesystem.TestFileSystem)
class TestFile extends FsTest {
	function testOpenRead(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/bytes.bin', Read, (e, file) -> {
				if(noException(e))
					file.getOffset((e, r) -> {
						if(noException(e) && equals(0, Int64.toInt(r))) {
							var expected = bytesBinContent();
							var bufOffset = 5;
							var buf = Bytes.alloc(expected.length + bufOffset);
							var firstReadLength = 10;
							//read less than EOF
							file.read(buf, bufOffset, firstReadLength, (e, r) -> {
								if(noException(e) && equals(firstReadLength, r)) {
									var expectedRead = expected.sub(0, r);
									var actualRead = buf.sub(bufOffset, r);
									if(equals(0, expectedRead.compare(actualRead))) {
										bufOffset += r;
										//read more than EOF
										file.read(buf, bufOffset, expected.length, (e, r) -> {
											if(noException(e) && equals(expected.length - firstReadLength, r)) {
												var expectedRead = expected.sub(firstReadLength, r);
												var actualRead = buf.sub(bufOffset, r);
												if(equals(0, expectedRead.compare(actualRead))) {
													//read after EOF
													file.read(buf, 0, 1, (e, r) -> {
														if(noException(e) && equals(0, r))
															file.close((e, _) -> noException(e));
													});
												}
											}
										});
									}
								}
							});
						}
					});
			}),
			//Buffer is too small
			FileSystem.openFile('test-data/bytes.bin', Read, (e, file) -> {
				if(noException(e)) {
					var buf = Bytes.alloc(1);
					file.read(buf, 0, 10, (e, _) -> {
						assertType(e, FsException, e -> equals('test-data/bytes.bin', e.path.toString()));
					});
				}
			}),
			//Read non-existent
			FileSystem.openFile('test-data/temp/non-existent', Read, (e, r) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non-existent', e.path.toString());
				});
			})
		);
	}

	@:depends(testOpenRead)
	function testIsEof(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/bytes.bin', Read, (e, file) -> {
				var buf = bytesBinContent();
				file.read(buf, 0, buf.length + 1, (e, r) -> {
					file.isEof((e, r) -> {
						if(noException(e)) {
							isTrue(r);
							file.close((_, _) -> {});
						}
					});
				});
			})
		);
	}
}