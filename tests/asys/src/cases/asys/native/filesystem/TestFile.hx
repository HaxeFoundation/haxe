package cases.asys.native.filesystem;

import haxe.io.BytesOutput;
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
				if(noException(e)) {
					var expected = bytesBinContent();
					var bufOffset = 5;
					var buf = Bytes.alloc(expected.length + bufOffset);
					var firstReadLength = 10;
					//read less than EOF
					file.read(buf, bufOffset, firstReadLength, (e, r) -> {
						if(noException(e) && equals(firstReadLength, r)) {
							var expectedRead = expected.sub(0, r);
							var actualRead = buf.sub(bufOffset, r);
							if(same(expectedRead, actualRead)) {
								bufOffset += r;
								//read more than EOF
								file.read(buf, bufOffset, expected.length, (e, r) -> {
									if(noException(e) && equals(expected.length - firstReadLength, r)) {
										var expectedRead = expected.sub(firstReadLength, r);
										var actualRead = buf.sub(bufOffset, r);
										if(same(expectedRead, actualRead)) {
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
			}),
			//Buffer is too small
			FileSystem.openFile('test-data/bytes.bin', Read, (e, file) -> {
				if(noException(e)) {
					var buf = Bytes.alloc(1);
					file.read(buf, 0, 10, (e, _) -> {
						assertType(e, FsException, e -> equals('test-data/bytes.bin', e.path.toString()));
						file.close((_, _) -> {});
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

	function testOpenAppend(async:Async) {
		asyncAll(async,
			//existing file
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/append.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/append.bin', Append, (e, file) -> {
					if(noException(e)) {
						var data = bytes([3, 2, 1, 0]);
						var b = new BytesOutput();
						var bb = bytesBinContent();
						b.writeBytes(bb, 0, bb.length);
						b.writeBytes(data, 1, 2);
						var expected = b.getBytes();

						file.write(data, 1, 2, (e, r) -> {
							if(noException(e) && equals(2, r))
								file.close((e, _) -> {
									if(noException(e))
										FileSystem.readBytes('test-data/temp/append.bin', (_, r) -> {
											same(expected, r);
										});
								});
						});
					}
				});
			}),
			//non-existent file
			FileSystem.openFile('test-data/temp/non-existent.bin', Append, (e, file) -> {
				if(noException(e)) {
					var buffer = bytes([1, 2, 3, 4, 5]);
					//try to write more bytes than `buffer` contains.
					file.write(buffer, 2, buffer.length + 2, (e, r) -> {
						if(noException(e) && equals(buffer.length - 2, r))
							file.close((e, _) -> {
								if(noException(e))
									FileSystem.readBytes('test-data/temp/non-existent.bin', (_, r) -> {
										same(buffer.sub(2, buffer.length - 2), r);
									});
							});
					});
				}
			}),
			//in non-existent directory
			FileSystem.openFile('test-data/temp/non/existent.bin', Append, (e, file) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non/existent.bin', e.path.toString());
				});
			})
		);
	}

	@:depends(testOpenRead)
	function testSeek(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/bytes.bin', Read, (_, file) -> {
				var content = bytesBinContent();
				var buf = Bytes.alloc(10);
				// SeekSet
				file.seek(20, SeekSet, (e, _) -> {
					if(noException(e))
						file.read(buf, 0, buf.length, (_, _) -> {
							same(content.sub(20, buf.length), buf);
							// SeekMove
							file.seek(20, SeekMove, (e, _) -> {
								if(noException(e))
									file.read(buf, 0, buf.length, (_, _) -> {
										same(content.sub(20 + buf.length + 20, buf.length), buf);
										// SeekEnd
										file.seek(-20, SeekEnd, (e, _) -> {
											if(noException(e))
												file.read(buf, 0, buf.length, (_, _) -> {
													same(content.sub(content.length - 20, buf.length), buf);
													file.close((_, _) -> {});
												});
										});
									});
							});
						});
				});
			})
		);
	}

	@:depends(testOpenRead, testSeek)
	function testGetPosition(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/bytes.bin', Read, (_, file) -> {
				file.getPosition((e, r) -> {
					if(noException(e)) {
						equals(0, Int64.toInt(r));
						file.seek(20, (_, _) -> {
							file.getPosition((e, r) -> {
								if(noException(e))
									equals(20, Int64.toInt(r));
								file.close((_, _) -> {});
							});
						});
					}
				});
			})
		);
	}

	// @:depends(testSeek)
	// function testOpenAppendRead(async:Async) {
	// 	asyncAll(async,

	// 		//non-existent file
	// 		FileSystem.openFile('test-data/temp/non-existent.bin', AppendRead, (e, file) -> {
	// 			if(noException(e)) {
	// 				var buffer = bytes([1, 2, 3, 4, 5]);
	// 				file.write(buffer, 0, buffer.length, (e, r) -> {
	// 					if(noException(e) && equals(buffer.length, r)) {
	// 						file.seek(0, (e, _) -> {
	// 							var readBuf = Bytes.alloc(buffer.length);
	// 							file.read(readBuf, 0, buffer.length, (e, r) -> {
	// 								if(noException(e)) {
	// 									equals(buffer.length, r);
	// 									same(buffer, readBuf);
	// 									file.close((e, _) -> {
	// 										if(noException(e))
	// 											FileSystem.readBytes('test-data/temp/non-existent.bin', (_, r) -> {
	// 												same(buffer, r);
	// 											});
	// 									});
	// 								}
	// 							});
	// 						});
	// 					}
	// 				});
	// 			}
	// 		}),
	// 		//in non-existent directory
	// 		FileSystem.openFile('test-data/temp/non/existent.bin', AppendRead, (e, file) -> {
	// 			assertType(e, FsException, e -> {
	// 				equals('test-data/temp/non/existent.bin', e.path.toString());
	// 			});
	// 		})
	// 	);
	// }

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

	@:depends(testOpenAppend)
	function testWrite_OutOfBufferBounds(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/temp/write.oob', Append, (_, file) -> {
				var buf = bytes([1, 2, 3]);
				//offset negative
				file.write(buf, -1, buf.length, (e, _) -> {
					assertType(e, FsException, e -> equals('test-data/temp/write.oob', e.path.toString()));
					//offset >= buf.length
					file.write(buf, buf.length, buf.length, (e, _) -> {
						assertType(e, FsException, e -> equals('test-data/temp/write.oob', e.path.toString()));
						//length negative
						file.write(buf, buf.length, buf.length, (e, _) -> {
							assertType(e, FsException, e -> equals('test-data/temp/write.oob', e.path.toString()));
							file.close((_, _) -> {});
						});
					});
				});
			})
		);
	}
}