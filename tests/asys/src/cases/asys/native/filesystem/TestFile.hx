package cases.asys.native.filesystem;

import asys.native.filesystem.FilePermissions;
import haxe.io.BytesOutput;
import haxe.Int64;
import haxe.Callback;
import haxe.io.Bytes;
import asys.native.filesystem.FsException;
import asys.native.filesystem.FileSystem;
import asys.native.filesystem.File;

@:depends(
	cases.asys.native.filesystem.TestFilePath,
	cases.asys.native.filesystem.TestFilePermissions,
	cases.asys.native.filesystem.TestFileSystem
)
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

	@:depends(testGetPosition)
	function testOpenAppendRead(async:Async) {
		asyncAll(async,
			//existing file
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/append-read.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/append-read.bin', AppendRead, (e, file) -> {
					if(noException(e)) {
						file.getPosition((e, r) -> {
							if(noException(e)) {
								equals(0, Int64.toInt(r));
								var data = bytes([3, 2, 1, 0]);
								var b = new BytesOutput();
								var bb = bytesBinContent();
								b.writeBytes(bb, 0, bb.length);
								b.writeBytes(data, 1, 2);
								var expected = b.getBytes();
								file.write(data, 1, 2, (e, r) -> {
									if(noException(e)) {
										equals(2, r);
										file.getPosition((e, r) -> {
											equals(2, Int64.toInt(r));
											file.close((e, _) -> {
												if(noException(e))
													FileSystem.readBytes('test-data/temp/append-read.bin', (_, r) -> {
														same(expected, r);
													});
											});
										});
									}
								});
							}
						});
					}
				});
			}),
			//non-existent file
			FileSystem.openFile('test-data/temp/non-existent.bin', AppendRead, (e, file) -> {
				if(noException(e)) {
					var buffer = bytes([1, 2, 3, 4, 5]);
					file.write(buffer, 0, buffer.length, (e, r) -> {
						if(noException(e) && equals(buffer.length, r)) {
							file.seek(0, (e, _) -> {
								var readBuf = Bytes.alloc(buffer.length);
								file.read(readBuf, 0, buffer.length, (e, r) -> {
									if(noException(e)) {
										equals(buffer.length, r);
										same(buffer, readBuf);
										file.close((e, _) -> {
											if(noException(e))
												FileSystem.readBytes('test-data/temp/non-existent.bin', (_, r) -> {
													same(buffer, r);
												});
										});
									}
								});
							});
						}
					});
				}
			}),
			//in non-existent directory
			FileSystem.openFile('test-data/temp/non/existent.bin', AppendRead, (e, file) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non/existent.bin', e.path.toString());
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

	function testOpenReadWrite(async:Async) {
		asyncAll(async,
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/read-write.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/read-write.bin', ReadWrite, (e, file) -> {
					if(noException(e)) {
						var expected = bytesBinContent();
						var buf = Bytes.alloc(10);
						file.read(buf, 0, buf.length, (e, bytesRead) -> {
							if(noException(e)) {
								equals(buf.length, bytesRead);
								same(expected.sub(0, buf.length), buf);
								buf = bytes([100, 50, 25]);
								expected.blit(bytesRead, buf, 0, buf.length);
								file.write(buf, 0, buf.length, (e, r) -> {
									if(noException(e) && equals(buf.length, r)) {
										file.close((e, _) -> {
											if(noException(e))
												FileSystem.readBytes('test-data/temp/read-write.bin', (_, r) -> {
													same(expected, r);
												});
										});
									}
								});
							}
						});
					}
				});
			}),
			FileSystem.openFile('test-data/temp/non-existent', ReadWrite, (e, _) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non-existent', e.path.toString());
				});
			})
		);
	}

	function testOpenWrite(async:Async) {
		asyncAll(async,
			//existing file
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/write.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/write.bin', Write, (e, file) -> {
					if(noException(e)) {
						var data = bytes([99, 88, 77]);
						file.write(data, 0, data.length, (e, r) -> {
							if(noException(e)) {
								equals(data.length, r);
								file.close((e, _) -> {
									if(noException(e))
										FileSystem.readBytes('test-data/temp/write.bin', (_, r) -> {
											same(data, r);
										});
								});
							}
						});
					}
				});
			}),
			//non-existent file
			FileSystem.openFile('test-data/temp/non-existent', Write, (e, file) -> {
				if(noException(e)) {
					var data = bytes([66, 55, 44]);
					file.write(data, 0, data.length, (e, r) -> {
						if(noException(e)) {
							equals(data.length, r);
							file.close((e, _) -> {
								if(noException(e))
									FileSystem.readBytes('test-data/temp/non-existent', (_, r) -> {
										same(data, r);
									});
							});
						}
					});
				}
			}),
			//exceptions
			FileSystem.openFile('test-data/temp/non/existent', Write, (e, _) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non/existent', e.path.toString());
				});
			})
		);
	}

	function testOpenWriteX(async:Async) {
		asyncAll(async,
			//existing file
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/writeX.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/writeX.bin', WriteX, (e, _) -> {
					assertType(e, FsException, e -> {
						equals('test-data/temp/writeX.bin', e.path.toString());
					});
				});
			}),
			//non-existent file
			FileSystem.openFile('test-data/temp/non-existent', WriteX, (e, file) -> {
				if(noException(e)) {
					var data = bytes([12, 34, 56, 78]);
					file.write(data, 0, data.length, (e, r) -> {
						if(noException(e)) {
							equals(data.length, r);
							file.close((e, _) -> {
								if(noException(e))
									FileSystem.readBytes('test-data/temp/non-existent', (_, r) -> {
										same(data, r);
									});
							});
						}
					});
				}
			}),
			//exceptions
			FileSystem.openFile('test-data/temp/non/existent', WriteX, (e, _) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non/existent', e.path.toString());
				});
			})
		);
	}

	@:depends(testSeek)
	function testOpenWriteRead(async:Async) {
		asyncAll(async,
			//existing file
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/write-read.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/write-read.bin', WriteRead, (e, file) -> {
					if(noException(e)) {
						var readBuf = Bytes.alloc(10);
						file.read(readBuf, 0, readBuf.length, (e, r) -> {
							if(noException(e)) {
								equals(0, r);
								same(Bytes.alloc(10), readBuf);
								var writeBuf = bytes([5, 7, 8, 9]);
								file.write(writeBuf, 0, writeBuf.length, (e, r) -> {
									if(noException(e)) {
										equals(writeBuf.length, r);
										file.seek(0, (_, _) -> {
											file.read(readBuf, 0, writeBuf.length, (e, r) -> {
												if(noException(e)) {
													equals(writeBuf.length, r);
													same(writeBuf, readBuf.sub(0, writeBuf.length));
													file.close((e, _) -> {
														if(noException(e))
															FileSystem.readBytes('test-data/temp/write-read.bin', (e, r) -> {
																same(writeBuf, r);
															});
													});
												}
											});
										});
									}
								});
							}
						});
					}
				});
			}),
			//non-existent file
			FileSystem.openFile('test-data/temp/non-existent', WriteRead, (e, file) -> {
				if(noException(e)) {
					var data = bytes([12, 34, 56, 78]);
					file.write(data, 0, data.length, (e, r) -> {
						if(noException(e)) {
							equals(data.length, r);
							file.seek(0, (_, _) -> {
								if(noException(e)) {
									var buf = Bytes.alloc(data.length);
									file.read(buf, 0, data.length, (e, r) -> {
										if(noException(e)) {
											equals(data.length, r);
											same(data, buf);
											file.close((e, _) -> {
												if(noException(e))
													FileSystem.readBytes('test-data/temp/non-existent', (_, r) -> {
														same(data, r);
													});
											});
										}
									});
								}
							});
						}
					});
				}
			}),
			//exceptions
			FileSystem.openFile('test-data/temp/non/existent', WriteRead, (e, _) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non/existent', e.path.toString());
				});
			})
		);
	}

	@:depends(testSeek)
	function testOpenWriteReadX(async:Async) {
		asyncAll(async,
			//existing file
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/write-readX.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/write-readX.bin', WriteReadX, (e, file) -> {
					assertType(e, FsException, e -> {
						equals('test-data/temp/write-readX.bin', e.path.toString());
					});
				});
			}),
			//non-existent file
			FileSystem.openFile('test-data/temp/non-existent', WriteReadX, (e, file) -> {
				if(noException(e)) {
					var data = bytes([12, 34, 56, 78]);
					file.write(data, 0, data.length, (e, r) -> {
						if(noException(e)) {
							equals(data.length, r);
							file.seek(0, (_, _) -> {
								if(noException(e)) {
									var buf = Bytes.alloc(data.length);
									file.read(buf, 0, data.length, (e, r) -> {
										if(noException(e)) {
											equals(data.length, r);
											same(data, buf);
											file.close((e, _) -> {
												if(noException(e))
													FileSystem.readBytes('test-data/temp/non-existent', (_, r) -> {
														same(data, r);
													});
											});
										}
									});
								}
							});
						}
					});
				}
			}),
			//exceptions
			FileSystem.openFile('test-data/temp/non/existent', WriteReadX, (e, _) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non/existent', e.path.toString());
				});
			})
		);
	}

	function testOpenOverwrite(async:Async) {
		asyncAll(async,
			//existing file
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/overwrite.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/overwrite.bin', Overwrite, (e, file) -> {
					if(noException(e)) {
						var data = bytes([99, 88, 77]);
						file.write(data, 0, data.length, (e, r) -> {
							if(noException(e)) {
								equals(data.length, r);
								file.close((e, _) -> {
									if(noException(e))
										FileSystem.readBytes('test-data/temp/overwrite.bin', (_, r) -> {
											var expected = bytesBinContent();
											expected.blit(0, data, 0, data.length);
											same(expected, r);
										});
								});
							}
						});
					}
				});
			}),
			//non-existent file
			FileSystem.openFile('test-data/temp/non-existent', Overwrite, (e, file) -> {
				if(noException(e)) {
					var data = bytes([66, 55, 44]);
					file.write(data, 0, data.length, (e, r) -> {
						if(noException(e)) {
							equals(data.length, r);
							file.close((e, _) -> {
								if(noException(e))
									FileSystem.readBytes('test-data/temp/non-existent', (_, r) -> {
										same(data, r);
									});
							});
						}
					});
				}
			}),
			//exceptions
			FileSystem.openFile('test-data/temp/non/existent', Overwrite, (e, _) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non/existent', e.path.toString());
				});
			})
		);
	}

	@:depends(testSeek)
	function testOpenOverwriteRead(async:Async) {
		asyncAll(async,
			//existing file
			FileSystem.copyFile('test-data/bytes.bin', 'test-data/temp/overwrite-read.bin', (_, _) -> {
				FileSystem.openFile('test-data/temp/overwrite-read.bin', OverwriteRead, (e, file) -> {
					if(noException(e)) {
						var readBuf = Bytes.alloc(10);
						var content = bytesBinContent();
						file.read(readBuf, 0, readBuf.length, (e, r) -> {
							if(noException(e)) {
								equals(readBuf.length, r);
								same(content.sub(0, readBuf.length), readBuf);
								var writeBuf = bytes([5, 7, 8, 9]);
								file.write(writeBuf, 0, writeBuf.length, (e, r) -> {
									if(noException(e)) {
										equals(writeBuf.length, r);
										file.seek(readBuf.length, (_, _) -> {
											file.read(readBuf, 0, writeBuf.length, (e, r) -> {
												if(noException(e)) {
													equals(writeBuf.length, r);
													same(writeBuf, readBuf.sub(0, writeBuf.length));
													file.close((e, _) -> {
														if(noException(e))
															FileSystem.readBytes('test-data/temp/overwrite-read.bin', (e, r) -> {
																content.blit(readBuf.length, writeBuf, 0, writeBuf.length);
																same(content, r);
															});
													});
												}
											});
										});
									}
								});
							}
						});
					}
				});
			}),
			//non-existent file
			FileSystem.openFile('test-data/temp/non-existent', OverwriteRead, (e, file) -> {
				if(noException(e)) {
					var data = bytes([12, 34, 56, 78]);
					file.write(data, 0, data.length, (e, r) -> {
						if(noException(e)) {
							equals(data.length, r);
							file.seek(0, (_, _) -> {
								if(noException(e)) {
									var buf = Bytes.alloc(data.length);
									file.read(buf, 0, data.length, (e, r) -> {
										if(noException(e)) {
											equals(data.length, r);
											same(data, buf);
											file.close((e, _) -> {
												if(noException(e))
													FileSystem.readBytes('test-data/temp/non-existent', (_, r) -> {
														same(data, r);
													});
											});
										}
									});
								}
							});
						}
					});
				}
			}),
			//exceptions
			FileSystem.openFile('test-data/temp/non/existent', OverwriteRead, (e, _) -> {
				assertType(e, FsException, e -> {
					equals('test-data/temp/non/existent', e.path.toString());
				});
			})
		);
	}

	//TODO create a test which actually tests `flush` behavior
	@:depends(testOpenWrite)
	function testFlush(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/temp/flush', Write, (e, file) -> {
				var data = bytes([123, 234, 56]);
				file.write(data, 0, data.length, (_, _) -> {
					file.flush((e, _) -> {
						if(noException(e))
							file.close((_, _) -> {});
					});
				});
			})
		);
	}

	//TODO create a test which actually tests `sync` behavior
#if !php
	@:depends(testOpenWrite)
	function testSync(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/temp/sync', Write, (e, file) -> {
				var data = bytes([123, 234, 56]);
				file.write(data, 0, data.length, (_, _) -> {
					file.sync((e, _) -> {
						if(noException(e))
							file.close((_, _) -> {});
					});
				});
			})
		);
	}
#end

	@:depends(testOpenRead)
	function testInfo(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/sub/hello.world', Read, (_, file) -> {
				file.info((e, r) -> {
					if(noException(e)) {
						equals(13, r.size);
						isTrue(r.isFile());
						isFalse(r.isDirectory());
						isFalse(r.isSymbolicLink());
					}
					file.close((_, _) -> {});
				});
			})
		);
	}

	@:depends(testOpenWrite, testInfo)
	function testPermissions(async:Async) {
		asyncAll(async,
			FileSystem.openFile('test-data/temp/set-perm', Write, (_, file) -> {
				var mode:FilePermissions = [0, 7, 6, 5];
				file.setPermissions(mode, (e, r) -> {
					if(noException(e))
						file.info((_, r) -> {
							isTrue(mode == r.mode & mode);
							file.close((_, _) -> {});
						});
				});
			})
		);
	}

	@:depends(testInfo, testOpenWrite)
	function testSetOwner(async:Async) {
		if(isWindows) {
			pass();
			return;
		}

		asyncAll(async,
			FileSystem.openFile('test-data/temp/set-owner', Write, (_, file) -> {
				file.info((_, r) -> {
					file.setOwner(r.userId, r.groupId, (e, _) -> {
						noException(e);
						file.close((_, _) -> {});
					});
				});
			})
		);
	}

	@:depends(testOpenReadWrite)
	function testResize(async:Async) {
		asyncAll(async,
			FileSystem.writeString('test-data/temp/resize1', 'hello', (_, _) -> {
				FileSystem.openFile('test-data/temp/resize1', ReadWrite, (_, file) -> {
					file.resize(2, (e, r) -> {
						if(noException(e)) {
							var buf = Bytes.alloc(100);
							file.read(buf, 0, buf.length, (e, r) -> {
								if(noException(e)) {
									equals(2, r);
									same(bytes(['h'.code, 'e'.code]), buf.sub(0, 2));
								}
								file.close((_, _) -> {});
							});
						}
					});
				});
			}),
			FileSystem.writeString('test-data/temp/resize2', 'hi', (_, _) -> {
				FileSystem.openFile('test-data/temp/resize2', ReadWrite, (_, file) -> {
					file.resize(10, (e, r) -> {
						if(noException(e)) {
							var buf = Bytes.alloc(100);
							file.read(buf, 0, buf.length, (e, r) -> {
								if(noException(e)) {
									equals(10, r);
									var expected = Bytes.alloc(10);
									expected.set(0, 'h'.code);
									expected.set(1, 'i'.code);
									same(expected, buf.sub(0, 10));
								}
								file.close((_, _) -> {});
							});
						}
					});
				});
			})
		);
	}

	@:depends(testOpenWrite, testInfo)
	function testSetTimes(async:Async) {
		var modificationTime = Std.int(Date.fromString('2020-01-01 00:01:02').getTime() / 1000);
		var accessTime = Std.int(Date.fromString('2020-02-03 04:05:06').getTime() / 1000);
		asyncAll(async,
			FileSystem.openFile('test-data/temp/set-times', Write, (_, file) -> {
				file.setTimes(accessTime, modificationTime, (e, r) -> {
					if(noException(e))
						file.info((_, r) -> {
							equals(modificationTime, r.modificationTime);
							equals(accessTime, r.accessTime);
							file.close((_, _) -> {});
						});
				});
			})
		);
	}

	@:depends(testOpenWrite)
	function testLock(async:Async) {
		//TODO: proper test for File.lock
		if(Sys.systemName() != 'Linux') {
			pass();
			return;
		}

		asyncAll(async,
			FileSystem.openFile('test-data/temp/file.lock', Write, (_, file) -> {
				file.lock(Exclusive, false, (e, r) -> {
					if(noException(e) && isTrue(r)) {
						var lockedExternally = 0 == Sys.command('flock', ['-n', 'test-data/temp/file.lock', '-c', 'echo']);
						isFalse(lockedExternally);
						file.lock(Unlock, (e, r) -> {
							if(noException(e) && isTrue(r)) {
								var lockedExternally = 0 == Sys.command('flock', ['-n', 'test-data/temp/file.lock', '-c', 'echo']);
								isTrue(lockedExternally);
							}
							file.close((_, _) -> {});
						});
					}
				});
			})
		);
	}

	@:depends(testSeek, testOpenWriteRead)
	function testFileSystem_tmpFile(async:Async) {
		asyncAll(async,
			FileSystem.tempFile((e, file) -> {
				if(noException(e)) {
					var path = file.path;
					FileSystem.check(path, Exists, (_, r) -> {
						if(isTrue(r)) {
							var writeBuf = bytes([0, 1, 2, 3]);
							file.write(writeBuf, 0, writeBuf.length, (_, r) -> {
								if(equals(writeBuf.length, r)) {
									file.seek(0, (_, _) -> {
										var readBuf = Bytes.alloc(writeBuf.length);
										file.read(readBuf, 0, readBuf.length, (_, r) -> {
											same(writeBuf, readBuf);
											equals(readBuf.length, r);
											file.close((_, _) -> {
												FileSystem.check(path, Exists, (_, r) -> isFalse(r));
											});
										});
									});
								}
							});
						}
					});
				}
			})
		);
	}
}