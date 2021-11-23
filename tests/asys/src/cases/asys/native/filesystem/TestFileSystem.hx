package cases.asys.native.filesystem;

import haxe.PosInfos;
import haxe.io.Path;
import asys.native.filesystem.FilePermissions;
import haxe.NoData;
import asys.native.filesystem.Callback;
import asys.native.filesystem.FileOpenFlag;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;
import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;

@:depends(
	cases.asys#if (java && !jvm) ._native #else .native #end.filesystem.TestFilePath,
	cases.asys#if (java && !jvm) ._native #else .native #end.filesystem.TestFilePermissions
)
class TestFileSystem extends FsTest {

	function testReadBytes(async:Async) {
		asyncAll(async,
			FileSystem.readBytes('test-data/bytes.bin', (e, r) -> {
				if(noException(e))
					same(bytesBinContent(), r);
			}),
			FileSystem.readBytes('test-data/', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('test-data', e.path));
			}),
			FileSystem.readBytes('non-existent', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
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
				assertType(e, FsException, e -> equalPaths('test-data', e.path));
			}),
			FileSystem.readString('non-existent', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
			})
		);
	}

	@:depends(testReadString)
	function testWriteString(async:Async) {
		function writeAndCheck(textToWrite:String, expectedContent:String, flag:FileOpenFlag<Dynamic>, callback:(ok:Bool)->Void, ?pos:PosInfos) {
			FileSystem.writeString('test-data/temp/test.txt', textToWrite, flag, (e, _) -> {
				if(noException(e, pos))
					FileSystem.readString('test-data/temp/test.txt', (e, r) -> {
						if(noException(e, pos))
							callback(equals(expectedContent, r, pos))
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
					assertType(e, FsException, e -> equalPaths(path, e.path));
				});
			}
		);
	}

	@:depends(testReadBytes)
	function testWriteBytes(async:Async) {
		function writeAndCheck(bytesToWrite:Bytes, expectedContent:Bytes, flag:FileOpenFlag<Dynamic>, callback:(ok:Bool)->Void, ?pos:PosInfos) {
			FileSystem.writeBytes('test-data/temp/test.bin', bytesToWrite, flag, (e, _) -> {
				if(noException(e, pos))
					FileSystem.readBytes('test-data/temp/test.bin', (e, r) -> {
						if(noException(e, pos))
							callback(same(expectedContent, r, pos))
						else
							callback(true);
					})
				else
					callback(false);
			});
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
					assertType(e, FsException, e -> equalPaths(path, e.path));
				});
			}
		);
	}

	@:depends(testLink,testIsLink)
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
			// #if !cs //TODO: implement these checks depending on .NET/mono versions
			FileSystem.check('test-data/temp', Writable, (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			FileSystem.check('test-data/temp', Writable | Readable, (e, r) -> {
				if(noException(e))
					isTrue(r);
			}),
			// #end
			#if !cs
				//too much hassle making windows links to work across different machines (CI, local PC, netowrk share etc)
				if(!isWindows)
					FileSystem.link('non-existent', 'test-data/temp/faulty-link', (_, _) -> {
						FileSystem.isLink('test-data/temp/faulty-link', (e, r) -> {
							if(noException(e) && isTrue(r))
								FileSystem.check('test-data/temp/faulty-link', Exists, (e, r) -> {
									if(noException(e))
										isFalse(r);
								});
						});
					}),
			#end
			FileSystem.check('non-existent', Exists, (e, r) -> {
				if(noException(e))
					isFalse(r);
			})
		);
		if(!isWindows) {
			asyncAll(async,
				FileSystem.check('/bin', Readable, (e, r) -> {
					if(noException(e))
						isTrue(r);
				}),
				#if !cs //TODO: implement these checks depending on .NET/mono versions
					FileSystem.check('/bin', Writable, (e, r) -> {
						if(noException(e))
							isFalse(r);
					}),
					FileSystem.check('/bin', Readable | Writable, (e, r) -> {
						if(noException(e))
							isFalse(r);
					}),
				#end
				FileSystem.check('/bin', Exists, (e, r) -> {
					if(noException(e))
						isTrue(r);
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
			}),
			FileSystem.isDirectory('non-existent', (e, r) -> {
				if(noException(e))
					isFalse(r);
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
			}),
			FileSystem.isFile('non-existent', (e, r) -> {
				if(noException(e))
					isFalse(r);
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
				assertType(e, FsException, e -> equalPaths('test-data/temp/non/existent', e.path));
			}),
			FileSystem.createDirectory('test-data/temp/non-existent1/non-existent2/non-existent3', true, (e, r) -> {
				if(noException(e))
					FileSystem.isDirectory('test-data/temp/non-existent1/non-existent2/non-existent3', (e, r) -> isTrue(r));
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
										FileSystem.move('test-data/temp/dir2/file', 'test-data/temp/moved/file', true, (e, r) -> {
											if(noException(e))
												FileSystem.readString('test-data/temp/moved/file', (e, r) -> equals('world', r));
										});
									}),
									//disable overwrite
									createData('test-data/temp/dir3', 'unexpected', () -> {
										FileSystem.move('test-data/temp/dir3/file', 'test-data/temp/moved/file', false, (e, r) -> {
											isOfType(e, FsException);
										});
									}),
									//non-empty directory
									createData('test-data/temp/dir4', 'hello', () -> {
										FileSystem.move('test-data/temp/dir4', 'test-data/temp/moved', true, (e, r) -> {
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
				assertType(e, FsException, e -> equalPaths('test-data/temp/non/existent', e.path));
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
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
			}),
			FileSystem.deleteFile('test-data/temp', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('test-data/temp', e.path));
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
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
			}),
			FileSystem.writeString('test-data/temp/file', '', (e, r) -> {
				FileSystem.deleteDirectory('test-data/temp/file', (e, r) -> {
					assertType(e, FsException, e -> equalPaths('test-data/temp/file', e.path));
				});
			}),
			FileSystem.createDirectory('test-data/temp/non-empty', (e, r) -> {
				FileSystem.writeString('test-data/temp/non-empty/file', '', (e, r) -> {
					FileSystem.deleteDirectory('test-data/temp/non-empty', (e, r) -> {
						assertType(e, FsException, e -> equalPaths('test-data/temp/non-empty', e.path));
					});
				});
			})
		);
	}

	function testInfo(async:Async) {
		asyncAll(async,
			FileSystem.info('test-data/sub/hello.world', (e, r) -> {
				if(noException(e)) {
					equals(13, r.size);
					isTrue(r.mode.isFile());
					isFalse(r.mode.isDirectory());
					#if !cs
						isFalse(r.mode.isLink());
					#end
				}
			}),
			#if !cs
				FileSystem.info('test-data/symlink', (e, r) -> {
					if(noException(e)) {
						equals(13, r.size);
						isTrue(r.mode.isFile());
						isFalse(r.mode.isDirectory());
						isFalse(r.mode.isLink());
					}
				}),
			#end
			FileSystem.info('test-data/sub', (e, r) -> {
				if(noException(e)) {
					isFalse(r.mode.isFile());
					isTrue(r.mode.isDirectory());
					#if !cs
						isFalse(r.mode.isLink());
					#end
				}
			}),
			FileSystem.info('non-existent', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
			})
		);
	}

	@:depends(testWriteString, testInfo)
	function testSetPermissions(async:Async) {
		if(isWindows #if cs || true #end) {
			pass();
			async.done();
			return;
		}

		asyncAll(async,
			FileSystem.writeString('test-data/temp/perm', '', (_, _) -> {
				var permissions:FilePermissions = [0, 7, 6, 5];
				FileSystem.setPermissions('test-data/temp/perm', permissions, (e, r) -> {
					if(noException(e))
						FileSystem.info('test-data/temp/perm', (_, r) -> {
							isTrue(r.mode.has(permissions));
						});
				});
			}),
			FileSystem.setPermissions('non-existent', [0, 7, 7, 7], (e, r) -> {
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
			})
		);
	}

	@:depends(testWriteString,testInfo)
	function testSetOwner(async:Async) {
		if(isWindows #if cs || true #end) {
			pass();
			async.done();
			return;
		}

		asyncAll(async,
			FileSystem.writeString('test-data/temp/set-owner', '', (e, r) -> {
				FileSystem.info('test-data/temp/set-owner', (e, r) -> {
					FileSystem.setOwner('test-data/temp/set-owner', r.user, r.group, (e, _) -> {
						noException(e);
						FileSystem.setOwner('test-data/temp/non-existent', r.user, r.group, (e, r) -> {
							assertType(e, FsException, e -> equalPaths('test-data/temp/non-existent', e.path));
						});
					});
				});
			})
		);
	}


	@:depends(testWriteString,testReadString,testReadBytes)
	function testResize(async:Async) {
		asyncAll(async,
			FileSystem.writeString('test-data/temp/resize1', 'hello', (e, r) -> {
				FileSystem.resize('test-data/temp/resize1', 2, (e, r) -> {
					if(noException(e))
						FileSystem.readString('test-data/temp/resize1', (e, r) -> equals('he', r));
				});
			}),
			FileSystem.writeString('test-data/temp/resize2', 'hi', (e, r) -> {
				FileSystem.resize('test-data/temp/resize2', 10, (e, r) -> {
					if(noException(e)) {
						var expected = Bytes.alloc(10);
						expected.set(0, 'h'.code);
						expected.set(1, 'i'.code);
						FileSystem.readBytes('test-data/temp/resize2', (e, r) -> same(expected, r));
					}
				});
			}),
			FileSystem.resize('test-data/temp/non-existent-file', 10, (e, r) -> {
				if(noException(e)) {
					var expected = Bytes.alloc(10);
					FileSystem.readBytes('test-data/temp/non-existent-file', (e, r) -> same(expected, r));
				}
			}),
			FileSystem.resize('test-data/temp/non-existent-dir/file', 5, (e, r) -> {
				assertType(e, FsException, e -> equalPaths('test-data/temp/non-existent-dir/file', e.path));
			})
		);
	}

	@:depends(testInfo)
	function testSetTimes(async:Async) {
		var modificationTime = 1577826063; // 2019-12-31 21:01:03
		var accessTime = 1580691906; // 2020-02-03 01:05:06
		asyncAll(async,
			FileSystem.setTimes('test-data/sub/hello.world', accessTime, modificationTime, (e, r) -> {
				if(noException(e))
					FileSystem.info('test-data/sub/hello.world', (e, r) -> {
					#if eval
						// TODO:
						// The time is always set to a slightly (by 10-60 sec) different value.
						// Find out why. Perhaps it's a bug in OCaml luv library.
						isTrue(Math.abs(modificationTime - r.modificationTime) < 100);
						isTrue(Math.abs(modificationTime - r.modificationTime) < 100);
					#else
						equals(modificationTime, r.modificationTime);
						equals(accessTime, r.accessTime);
					#end
					});
			}),
			FileSystem.setTimes('test-data/temp/set-times-non-existent', accessTime, modificationTime, (e, r) -> {
				assertType(e, FsException, e -> equalPaths('test-data/temp/set-times-non-existent', e.path));
			}),
			FileSystem.setTimes('test-data/temp/non/existent/set-times', accessTime, modificationTime, (e, r) -> {
				assertType(e, FsException, e -> equalPaths('test-data/temp/non/existent/set-times', e.path));
			})
		);
	}

	function testIsLink(async:Async) {
		#if cs
			pass();
			async.done();
			return;
		#end

		asyncAll(async,
			//too much hassle making windows links to work across different machines (CI, local PC, netowrk share etc)
			if(!isWindows)
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
		#if cs
			pass();
			async.done();
			return;
		#end

		asyncAll(async,
			//too much hassle making windows links to work across different machines (CI, local PC, netowrk share etc)
			if(!isWindows)
				FileSystem.readLink('test-data/symlink', (e, r) -> {
					if(noException(e))
						equals('sub' + FilePath.SEPARATOR + 'hello.world', r.toString());
				}),
			FileSystem.readLink('test-data/sub/hello.world', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('test-data/sub/hello.world', e.path));
			}),
			FileSystem.readLink('non-existent', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
			})
		);
	}

	function testLinkInfo(async:Async) {
		#if cs
			pass();
			async.done();
			return;
		#end

		asyncAll(async,
			//too much hassle making windows links to work across different machines (CI, local PC, netowrk share etc)
			if(!isWindows)
				FileSystem.linkInfo('test-data/symlink', (e, r) -> {
					if(noException(e)) {
						isFalse(r.mode.isFile());
						isFalse(r.mode.isDirectory());
						isTrue(r.mode.isLink());
					}
				}),
			FileSystem.linkInfo('non-existent', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
			})
		);
	}

	@:depends(testReadLink, testIsLink, testReadString)
	function testLink(async:Async) {
		#if cs
			pass();
			async.done();
			return;
		#end

		asyncAll(async,
			//too much hassle making windows links to work across different machines (CI, local PC, netowrk share etc)
			if(!isWindows)
				FileSystem.link('../sub/hello.world', 'test-data/temp/symlink', SymLink, (e, r) -> {
					if(noException(e))
						FileSystem.readLink('test-data/temp/symlink', (e, r) -> {
							if(noException(e))
								equals('../sub/hello.world', r.toString());
						});
				}),
			//windows requires elevated mode or UAC disabled to create hard links
			if(!isWindows)
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
				assertType(e, FsException, e -> equalPaths('test-data/temp/non-existent/link', e.path));
			})
		);
	}

	@:depends(testLink,testInfo)
	function testSetLinkOwner(async:Async) {
		#if cs
			pass();
			async.done();
			return;
		#end
		if(isWindows) {
			pass();
			async.done();
			return;
		}

		asyncAll(async,
			FileSystem.link('../sub/hello.world', 'test-data/temp/set-link-owner', (e, r) -> {
				FileSystem.info('test-data/temp/set-link-owner', (e, r) -> {
					FileSystem.setLinkOwner('test-data/temp/set-link-owner', r.user, r.group, (e, _) -> {
						noException(e);
						FileSystem.setLinkOwner('test-data/temp/non-existent-link', r.user, r.group, (e, r) -> {
							assertType(e, FsException, e -> equalPaths('test-data/temp/non-existent-link', e.path));
						});
					});
				});
			})
		);
	}

	@:depends(testReadBytes, testReadString)
	function testCopyFile(async:Async) {
		asyncAll(async,
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/copy', (e, r) -> {
				if(noException(e))
					FileSystem.readBytes('test-data/temp/copy', (e, r) -> {
						if(noException(e) && same(bytesBinContent(), r)) {
							asyncAll(async,
								//disable overwrite
								FileSystem.copyFile('test-data/sub/hello.world', 'test-data/temp/copy', false, (e, r) -> {
									assertType(e, FsException, e -> {
										var path = e.path.toString();
										isTrue(path == 'test-data/sub/hello.world' || path == 'test-data/temp/copy');
									});
								}),
								//overwrite
								FileSystem.copyFile('test-data/sub/hello.world', 'test-data/temp/copy', (e, r) -> {
									if(noException(e))
										FileSystem.readString('test-data/temp/copy', (e, r) -> {
											if(noException(e))
												equals('Hello, world!', r);
										});
								})
							);
						}
					});
			}),
			FileSystem.copyFile('non-existent', 'test-data/temp/copy', (e, r) -> {
				assertType(e, FsException, e -> equalPaths('non-existent', e.path));
			}),
			FileSystem.copyFile('test-data/sub/hello.world', 'test-data/non-existent/copy', (e, r) -> {
				assertType(e, FsException, e -> {
					var path = e.path.toString();
					isTrue(path == 'test-data/sub/hello.world' || path == 'test-data/non-existent/copy');
				});
			})
		);
	}

	function testListDirectory(async:Async) {
		asyncAll(async,
			FileSystem.listDirectory('test-data', (e, r) -> {
				if(noException(e)) {
					var stringified = r.map(p -> p.toString());
					var expected = ['sub', 'symlink-dir', 'temp', 'bytes.bin', 'symlink'];
					expected.sort(Reflect.compare);
					stringified.sort(Reflect.compare);
					same(expected, stringified);
				}
			}),
			FileSystem.listDirectory('test-data/temp/non-existent', (e, r) -> {
				assertType(e, FsException, e -> {
					equalPaths('test-data/temp/non-existent', e.path);
				});
			})
		);
	}

	@:depends(testInfo)
	function testUniqueDirectory(async:Async) {
		var permissions:FilePermissions = [0, 7, 6, 5];
		asyncAll(async,
			FileSystem.uniqueDirectory('test-data/temp/non-existent/dir1', permissions, true, (e, path) -> {
				if(noException(e))
					FileSystem.info(path, (e, r) -> {
						if(noException(e)) {
							isTrue(r.mode.isDirectory());
							#if !cs
								isTrue(r.mode.has(permissions));
							#end
						}
					});
			}),
			FileSystem.uniqueDirectory('test-data/temp/non-existent-2/dir2', false, (e, path) -> {
				assertType(e, FsException, e -> {
					equalPaths('test-data/temp/non-existent-2/dir2', e.path);
				});
			})
		);
	}

	function testRealPath(async:Async) {
		var expected = Path.join([Sys.getCwd(), 'test-data', 'sub', 'hello.world']);

		asyncAll(async, {
			var p:FilePath = 'test-data/sub/.././../test-data////sub/hello.world';
			FileSystem.realPath(p, (e, p) -> {
				if(noException(e)) {
					equalPaths(expected, p.toString());
				}
			});
		},
		#if !cs //C# does not have API to resolve symlinks
		{
			var p:FilePath = 'test-data/symlink';
			//too much hassle making windows links to work across different machines (CI, local PC, netowrk share etc)
			if(!isWindows)
				FileSystem.realPath(p, (e, p) -> {
					if(noException(e)) {
						equalPaths(expected, p.toString());
					}
				});
		},
		#end
		{
			var p:FilePath = 'non-existent';
			FileSystem.realPath(p, (e, r) -> {
				assertType(e, FsException, e -> {
					isTrue(p == e.path);
				});
			});
		});
	}
}
