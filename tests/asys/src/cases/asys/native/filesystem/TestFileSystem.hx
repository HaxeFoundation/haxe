package cases.asys.native.filesystem;

import haxe.NoData;
import asys.native.filesystem.Callback;
import asys.native.filesystem.FileOpenFlag;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;
import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;

class TestFileSystem extends Test {
	function setup() {
		var tempDir = 'test-data/temp';
		//TODO: Find a way to create & cleanup `test-data/temp` directory without using old sys API
		if(!sys.FileSystem.exists(tempDir))
			sys.FileSystem.createDirectory(tempDir);
		switch sys.FileSystem.readDirectory(tempDir) {
			case []:
			case _:
				if(isWindows)
					Sys.command('rmdir', [tempDir, '/S', '/Q'])
				else
					Sys.command('rm', ['-rf', tempDir]);
				sys.FileSystem.createDirectory(tempDir);
		}
	}

	/**
	 * Expected content of `test-data/bytes.bin` file
	 */
	function bytesBinContent():Bytes {
		var data = Bytes.alloc(256);
		for(i in 0...data.length) data.set(i, i);
		return data;
	}

	function testReadBytes(async:Async) {
		asyncAll(async,
			FileSystem.readBytes('test-data/bytes.bin', (e, r) -> {
				if(noException(e))
					equals(0, bytesBinContent().compare(r));
			}),
			FileSystem.readBytes('test-data/', (e, r) -> {
				if(isOfType(e, FsException))
					equals('test-data/', cast(e, FsException).path.toString());
			}),
			FileSystem.readBytes('non-existent', (e, r) -> {
				if(isOfType(e, FsException))
					equals('non-existent', cast(e, FsException).path.toString());
			})
		);
	}

	function testReadString(async:Async) {
		asyncAll(async,
			FileSystem.readString('test-data/sub/hello.world', (e, r) -> {
				if(noException(e))
					equals('Hello, world!', r);
			}),
			FileSystem.readString('test-data/', (e, r) -> {
				if(isOfType(e, FsException))
					equals('test-data/', cast(e, FsException).path);
			}),
			FileSystem.readString('non-existent', (e, r) -> {
				if(isOfType(e, FsException))
					equals('non-existent', cast(e, FsException).path);
			})
		);
	}

	@:depends(testReadString)
	function testWriteString(async:Async) {
		function writeAndCheck(textToWrite:String, expectedContent:String, flag:FileOpenFlag<Dynamic>, callback:(ok:Bool)->Void) {
			FileSystem.writeString('test-data/temp/test.txt', textToWrite, flag, (e, _) -> {
				if(noException(e))
					FileSystem.readString('test-data/temp/test.txt', (e, r) -> {
						if(noException(e))
							callback(equals(expectedContent, r))
						else
							callback(false);
					})
				else
					callback(false);
			});
		}

		asyncAll(async,
			writeAndCheck('Hello, ', 'Hello, ', Write, ok -> {
				if(ok) writeAndCheck('world!', 'Hello, world!', Append, ok -> {
					if(ok) writeAndCheck('Goodbye', 'Goodbye', Write, ok -> {
						if(ok) writeAndCheck('Bye-', 'Bye-bye', Overwrite, ok -> {});
					});
				});
			}),
			{
				var path = 'test-data/temp/non-existent-dir/test.txt';
				FileSystem.writeString(path, '', (e, r) -> {
					if(isOfType(e, FsException))
						equals(path, cast(e, FsException).path.toString());
				});
			}
		);
	}

	@:depends(testReadBytes)
	function testWriteBytes(async:Async) {
		function writeAndCheck(bytesToWrite:Bytes, expectedContent:Bytes, flag:FileOpenFlag<Dynamic>, callback:(ok:Bool)->Void) {
			FileSystem.writeBytes('test-data/temp/test.bin', bytesToWrite, flag, (e, _) -> {
				if(noException(e))
					FileSystem.readBytes('test-data/temp/test.bin', (e, r) -> {
						if(noException(e))
							callback(equals(0, expectedContent.compare(r)))
						else
							callback(true);
					})
				else
					callback(false);
			});
		}
		function bytes(data:Array<Int>):Bytes {
			var b = Bytes.alloc(data.length);
			for (index => value in data) {
				b.set(index, value);
			}
			return b;
		}

		asyncAll(async,
			writeAndCheck(bytes([0, 1, 2]), bytes([0, 1, 2]), Write, ok -> {
				if(ok) writeAndCheck(bytes([3, 4, 5]), bytes([0, 1, 2, 3, 4, 5]), Append, ok -> {
					if(ok) writeAndCheck(bytes([6, 7, 8, 9]), bytes([6, 7, 8, 9]), Write, ok -> {
						if(ok) writeAndCheck(bytes([10, 11]), bytes([10, 11, 8, 9]), Overwrite, ok -> {});
					});
				});
			}),
			{
				var path = 'test-data/temp/non-existent-dir/test.bin';
				FileSystem.writeBytes(path, Bytes.alloc(1), (e, r) -> {
					if(isOfType(e, FsException))
						equals(path, cast(e, FsException).path.toString());
				});
			}
		);
	}

	function testCheck(async:Async) {
		asyncAll(async,
			FileSystem.check('test-data/sub', Exists, (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			FileSystem.check('test-data/sub/hello.world', Readable, (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			FileSystem.check('test-data/temp', Writable, (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			FileSystem.check('test-data/temp', Writable | Readable, (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			FileSystem.check('non-existent', Exists, (e, r) -> {
				if(noException(e))
					isFalse(r);
			})
		);
		if(!isWindows) {
			asyncAll(async,
				FileSystem.check('/bin', Exists, (e, r) -> {
					if(noException(e))
						isTrue(r);
				}),
				FileSystem.check('/bin', Readable, (e, r) -> {
					if(noException(e))
						isTrue(r);
				}),
				FileSystem.check('/bin', Writable, (e, r) -> {
					if(noException(e))
						isFalse(r);
				}),
				FileSystem.check('/bin', Readable | Writable, (e, r) -> {
					if(noException(e))
						isFalse(r);
				})
			);
		}
	}

	function testIsDirectory(async:Async) {
		asyncAll(async,
			FileSystem.isDirectory('test-data/sub', (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			FileSystem.isDirectory('test-data/sub/hello.world', (e, r) -> {
				if(noException(e))
					isFalse(r);
			}),
			FileSystem.isDirectory('test-data/symlink-dir', (e, r) -> {
				if(noException(e))
					isTrue(r);
			})
		);
	}

	function testIsFile(async:Async) {
		asyncAll(async,
			FileSystem.isFile('test-data/sub/hello.world', (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			FileSystem.isFile('test-data/sub', (e, r) -> {
				if(noException(e))
					isFalse(r);
			}),
			FileSystem.isFile('test-data/symlink', (e, r) -> {
				if(noException(e))
					isTrue(r);
			})
		);
	}

	@:depends(testIsDirectory)
	function testCreateDirectory(async:Async) {
		asyncAll(async,
			FileSystem.createDirectory('test-data/temp/dir', (e, r) -> {
				if(noException(e))
					FileSystem.isDirectory('test-data/temp/dir', (e, r) -> isTrue(r));
			}),
			FileSystem.createDirectory('test-data/temp/non/existent', (e, r) -> {
				if(isOfType(e, FsException))
					equals('test-data/temp/non/existent', cast(e, FsException).path.toString());
			}),
			FileSystem.createDirectory('test-data/temp/non-existent1/non-existent2', true, (e, r) -> {
				if(noException(e))
					FileSystem.isDirectory('test-data/temp/dir', (e, r) -> isTrue(r));
			})
		);
	}

	@:depends(testCreateDirectory, testWriteString, testReadString, testCheck)
	function testMove(async:Async) {
		function createData(path:String, fileContent:String, callback:()->Void) {
			FileSystem.createDirectory(path, (e, r) -> {
				FileSystem.writeString('$path/file', fileContent, (e, r) -> {
					callback();
				});
			});
		}

		asyncAll(async,
			//move directory
			createData('test-data/temp/dir1', 'hello', () -> {
				FileSystem.move('test-data/temp/dir1', 'test-data/temp/moved', (e, r) -> {
					if(noException(e))
						asyncAll(async,
							FileSystem.check('test-data/temp/dir1', Exists, (e, r) -> isFalse(r)),
							FileSystem.readString('test-data/temp/moved/file', (e, r) -> {
								equals('hello', r);
								asyncAll(async,
									//overwrite
									createData('test-data/temp/dir2', 'world', () -> {
										FileSystem.move('test-data/temp/dir2', 'test-data/temp/moved', (e, r) -> {
											if(noException(e))
												FileSystem.readString('test-data/temp/moved/file', (e, r) -> equals('world', r));
										});
									}),
									//disable overwrite
									createData('test-data/temp/dir3', 'unexpected', () -> {
										FileSystem.move('test-data/temp/dir3', 'test-data/temp/moved', false, (e, r) -> {
											isOfType(e, FsException);
										});
									})
								);
							})
						);
				});
			}),
			//move file
			createData('test-data/temp/dir4', 'hello', () -> {
				FileSystem.move('test-data/temp/dir4/file', 'test-data/temp/dir4/file2', (e, r) -> {
					if(noException(e))
						FileSystem.readString('test-data/temp/dir4/file2', (e, r) -> equals('hello', r));
				});
			}),
			//check exceptions
			FileSystem.move('test-data/temp/non/existent', 'test-data/temp/non-existent', (e, r) -> {
				if(isOfType(e, FsException))
					equals('test-data/temp/non/existent', cast(e, FsException).path.toString());
			})
		);
	}

	@:depends(testWriteString, testCheck)
	function testDeleteFile(async:Async) {
		asyncAll(async,
			FileSystem.writeString('test-data/temp/test.txt', '', (e, r) -> {
				FileSystem.deleteFile('test-data/temp/test.txt', (e, r) -> {
					if(noException(e))
						FileSystem.check('test-data/temp/test.txt', Exists, (e, r) -> isFalse(r));
				});
			}),
			FileSystem.deleteFile('non-existent', (e, r) -> {
				if(isOfType(e, FsException))
					equals('non-existent', cast(e, FsException).path.toString());
			}),
			FileSystem.deleteFile('test-data/temp', (e, r) -> {
				if(isOfType(e, FsException))
					equals('test-data/temp', cast(e, FsException).path.toString());
			})
		);
	}

	@:depends(testCreateDirectory, testWriteString, testCheck)
	function testDeleteDirectory(async:Async) {
		asyncAll(async,
			FileSystem.createDirectory('test-data/temp/del-dir', (e, r) -> {
				FileSystem.deleteDirectory('test-data/temp/del-dir', (e, r) -> {
					if(noException(e))
						FileSystem.check('test-data/temp/del-dir', Exists, (e, r) -> isFalse(r));
				});
			}),
			FileSystem.deleteDirectory('non-existent', (e, r) -> {
				if(isOfType(e, FsException))
					equals('non-existent', cast(e, FsException).path.toString());
			}),
			FileSystem.writeString('test-data/temp/file', '', (e, r) -> {
				FileSystem.deleteDirectory('test-data/temp/file', (e, r) -> {
					if(isOfType(e, FsException))
						equals('test-data/temp/file', cast(e, FsException).path.toString());
				});
			}),
			FileSystem.createDirectory('test-data/temp/non-empty', (e, r) -> {
				FileSystem.writeString('test-data/temp/non-empty/file', '', (e, r) -> {
					FileSystem.deleteDirectory('test-data/temp/non-empty', (e, r) -> {
						if(isOfType(e, FsException))
							equals('test-data/temp/non-empty', cast(e, FsException).path.toString());
					});
				});
			})
		);
	}

	function testIsLink(async:Async) {
		asyncAll(async,
			FileSystem.isLink('test-data/symlink', (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			FileSystem.isLink('test-data/sub/hello.world', (e, r) -> {
				if(noException(e))
					isFalse(r);
			}),
			FileSystem.isLink('test-data', (e, r) -> {
				if(noException(e))
					isFalse(r);
			}),
			FileSystem.isLink('non-existent', (e, r) -> {
				if(noException(e))
					isFalse(r);
			})
		);
	}

	function testReadLink(async:Async) {
		asyncAll(async,
			FileSystem.readLink('test-data/symlink', (e, r) -> {
				if(noException(e))
					equals('sub' + FilePath.SEPARATOR + 'hello.world', r);
			}),
			FileSystem.readLink('test-data/sub/hello.world', (e, r) -> {
				if(isOfType(e, FsException))
					equals('test-data/sub/hello.world', cast(e, FsException).path.toString());
			}),
			FileSystem.readLink('non-existent', (e, r) -> {
				if(isOfType(e, FsException))
					equals('non-existent', cast(e, FsException).path.toString());
			})
		);
	}

	@:depends(testReadLink, testReadBytes, testReadString)
	function testCopyFile(async:Async) {
		asyncAll(async,
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/copy', (e, r) -> {
				if(noException(e))
					FileSystem.readBytes('test-data/temp/copy', (e, r) -> {
						if(noException(e)) {
							equals(0, bytesBinContent().compare(r));
							asyncAll(async,
								//overwrite
								FileSystem.copyFile('test-data/sub/hello.world', 'test-data/temp/copy', (e, r) -> {
									if(noException(e))
										FileSystem.readString('test-data/temp/copy', (e, r) -> {
											if(noException(e))
												equals('Hello, world!', r);
										});
								}),
								//disable overwrite
								FileSystem.copyFile('test-data/sub/hello.world', 'test-data/temp/copy', false, (e, r) -> {
									if(isOfType(e, FsException)) {
										var path = cast(e, FsException).path.toString();
										isTrue(path == 'test-data/sub/hello.world' || path == 'test-data/temp/copy');
									}
								})
							);
						}
					});
			}),
			FileSystem.copyFile('non-existent', 'test-data/temp/copy', (e, r) -> {
				if(isOfType(e, FsException))
					equals('non-existent', cast(e, FsException).path.toString());
			}),
			FileSystem.copyFile('test-data/sub/hello.world', 'test-data/non-existent/copy', (e, r) -> {
				if(isOfType(e, FsException)) {
					var path = cast(e, FsException).path.toString();
					isTrue(path == 'test-data/sub/hello.world' || path == 'test-data/non-existent/copy');
				}
			})
		);
	}

	@:depends(testReadLink, testIsLink, testReadString)
	function testLink(async:Async) {
		asyncAll(async,
			FileSystem.link('../sub/hello.world', 'test-data/temp/symlink', SymLink, (e, r) -> {
				if(noException(e))
					FileSystem.readLink('test-data/temp/symlink', (e, r) -> {
						if(noException(e))
							equals('../sub/hello.world', r.toString());
					});
			}),
			FileSystem.link('test-data/sub/hello.world', 'test-data/temp/hardlink', HardLink, (e, r) -> {
				if(noException(e))
					FileSystem.isLink('test-data/temp/hardlink', (e, r) -> {
						if(noException(e))
							if(isFalse(r))
								FileSystem.readString('test-data/temp/hardlink', (e, r) -> {
									if(noException(e))
										equals('Hello, world!', r);
								});
					});
			}),
			FileSystem.link('../sub/hello.world', 'test-data/temp/non-existent/link', (e, r) -> {
				if(isOfType(e, FsException))
					equals('test-data/temp/non-existent/link', cast(e, FsException).path.toString());
			})
		);
	}
}
