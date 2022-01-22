import haxe.Constraints.Function;
import haxe.io.Bytes;
import cpp.uv.UVError;
import haxe.PosInfos;
import cpp.uv.Misc;
import cpp.uv.UVException;
import sys.thread.Thread;
import cpp.uv.File;

abstract Actions(Array<Function>) from Array<Function> {
	public function next(?p:PosInfos) {
		var fn = this.shift();
		if(fn != null) {
			Log.print('-----------', p);
			fn(this);
		}
	}
}

class FileSample extends UVSample {
	static final loop = Thread.current().events;

	public function run() {
		var actions:Actions = [
			createWriteSyncReadUnlink,
			mkdirRenameRmdir,
			mkdtempRmdir,
			mkstempUnlink,
			statFStat,
			statFs,
			truncate,
			copyFile,
			// sendFile, // Throws EBADF and idk why
			access,
			chmod,
			utime,
			linkSymlinkReadLinkRealPath,
			chown,
		];
		actions.next();
	}

	function handle(success:()->Void, ?p:PosInfos):(e:UVError)->Void {
		return e -> switch e {
			case UV_NOERR: success();
			case _: throw new UVException(e, p.fileName + ':' + p.lineNumber + ': ' + e.toString());
		}
	}

	function createFile(path:String, content:Bytes, callback:()->Void, ?pos:PosInfos) {
		File.open(loop, path, [CREAT(420),TRUNC,WRONLY], (e, file) -> handle(() -> {
			file.write(loop, content, 0, content.length, 0, (e, bytesWritten) -> handle(() -> {
				file.close(loop, handle(callback, pos));
			}, pos)(e));
		}, pos)(e));
	}

	function readFile(path:String, callback:(data:Bytes)->Void) {
		File.open(loop, path, [RDONLY], (e, file) -> handle(() -> {
			var buf = Bytes.alloc(10240);
			file.read(loop, buf, 0, 10240, 0, (e, bytesRead) -> handle(() -> {
				file.close(loop, handle(() -> {
					callback(buf.sub(0, bytesRead));
				}));
			})(e));
		})(e));
	}

	function deleteFiles(files:Array<String>, callback:()->Void) {
		var finished = 0;
		for(path in files) {
			File.unlink(loop, path, handle(() -> {
				++finished;
				if(finished == files.length)
					callback();
			}));
		}
	}

	function createWriteSyncReadUnlink(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file';
		print('Creating $path for writing...');
		File.open(loop, path, [CREAT(420), WRONLY], (e, file) -> handle(() -> {
			print('Writing...');
			var data = Bytes.ofString('Hello, world!');
			file.write(loop, data, 0, data.length, 0, (e, bytesWritten) -> handle(() -> {
				print('$bytesWritten bytes written: $data');
				print('fsync...');
				file.fsync(loop, handle(() -> {
					print('fdatasync...');
					file.fdataSync(loop, handle(() -> {
						file.close(loop, handle(() -> {
							print('closed $path');
							readUnlink(path, actions);
						}));
					}));
				}));
			})(e));
		})(e));
	}

	function readUnlink(path:String, actions:Actions) {
		print('Opening $path for reading...');
		File.open(loop, path, [RDONLY], (e, file) -> handle(() -> {
			print('Reading...');
			var buf = Bytes.alloc(1024);
			file.read(loop, buf, 0, 1024, 0, (e, bytesRead) -> handle(() -> {
				print('$bytesRead bytes read: ' + buf.toString());
				file.close(loop, handle(() -> {
					print('closed $path');
					unlink(path, actions);
				}));
			})(e));
		})(e));
	}

	function unlink(path:String, actions:Actions) {
		print('Unlinking $path...');
		File.unlink(loop, path, handle(() -> {
			actions.next();
		}));
	}

	function mkdirRenameRmdir(actions:Actions) {
		var path = Misc.tmpDir() + '/test-dir';
		var newPath = Misc.tmpDir() + '/test-dir2';
		print('Creating directory $path...');
		File.mkdir(loop, path, 511, handle(() -> {
			print('Renaming $path to $newPath...');
			File.rename(loop, path, newPath, handle(() -> {
				print('Removing directory $newPath...');
				File.rmdir(loop, newPath, handle(() -> {
					print('Done');
					actions.next();
				}));
			}));
		}));
	}

	function mkdtempRmdir(actions:Actions) {
		var tpl = Misc.tmpDir() + '/test-dir-XXXXXX';
		print('Creating temp directory with tpl $tpl...');
		File.mkdtemp(loop, tpl, (e, path) -> handle(() -> {
			print('Removing directory $path...');
			File.rmdir(loop, path, handle(() -> {
				print('Done');
				actions.next();
			}));
		})(e));
	}

	function mkstempUnlink(actions:Actions) {
		var tpl = Misc.tmpDir() + '/test-file-XXXXXX';
		print('Creating temp file with tpl $tpl...');
		File.mkstemp(loop, tpl, (e, file, path) -> handle(() -> {
			print('Closing $path...');
			file.close(loop, handle(() -> {
				print('Unlinking $path...');
				File.unlink(loop, path, handle(() -> {
					print('Done');
					actions.next();
				}));
			}));
		})(e));
	}

	function statFStat(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file';
		print('fstat on $path...');
		File.open(loop, path, [CREAT(420)], (e, file) -> handle(() -> {
			file.fstat(loop, (e, fstat) -> handle(() -> {
				print('got fstat: $fstat');
				file.close(loop, handle(() -> {
					print('stat on $path');
					File.stat(loop, path, (e, stat) -> handle(() -> {
						print('got stat: $stat');
						// TODO: jit error on I64 == I64
						// var ok = stat.dev == fstat.dev;
						// 	&& stat.mode == fstat.mode
						// 	&& stat.nlink == fstat.nlink
						// 	&& stat.uid == fstat.uid
						// 	&& stat.gid == fstat.gid
						// 	&& stat.rdev == fstat.rdev
						// 	&& stat.ino == fstat.ino
						// 	&& stat.size == fstat.size
						// 	&& stat.blksize == fstat.blksize
						// 	&& stat.blocks == fstat.blocks
						// 	&& stat.flags == fstat.flags
						// 	&& stat.gen == fstat.gen;
						// print('fstat equals stat: $ok');
						deleteFiles([path], () -> {
							print('Done');
							actions.next();
						});
					})(e));
				}));
			})(e));
		})(e));
	}

	function statFs(actions:Actions) {
		print('statfs on .');
		File.statFs(loop, '.', (e, stat) -> handle(() -> {
			print('got statfs: $stat');
			print('Done');
			actions.next();
		})(e));
	}

	function truncate(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-truncate';
		var content = '1234567890';
		print('Writing content for truncation at $path: $content');
		createFile(path, Bytes.ofString(content), () -> {
			File.open(loop, path, [WRONLY], (e, file) -> handle(() -> {
				print('truncating at 5...');
				file.ftruncate(loop, 5, handle(() -> {
					file.close(loop, handle(() -> {
						readFile(path, data -> {
							print('Content after truncation (length=${data.length}): $data');
							deleteFiles([path], () -> {
								print('Done');
								actions.next();
							});
						});
					}));
				}));
			})(e));
		});
	}

	function copyFile(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-copy';
		var newPath = '$path-copy';
		createFile(path, Bytes.ofString('123'), () -> {
			print('Copy $path to $newPath');
			File.copyFile(loop, path, newPath, [EXCL], handle(() -> {
				print('Copy success');
				deleteFiles([path, newPath], () -> {
					print('Done');
					actions.next();
				});
			}));
		});
	}

	function sendFile(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-send';
		var newPath = '$path-copy';
		createFile(path, Bytes.ofString('12345678'), () -> {
			File.open(loop, path, [RDONLY], (e, src) -> handle(() -> {
				File.open(loop, newPath, [CREAT(420), WRONLY], (e, dst) -> handle(() -> {
					print('sendFile from $path to $newPath...');
					src.sendFile(loop, dst, 0, 20, (e, outOffset) -> handle(() -> {
						print('sendfile stopped at $outOffset');
						src.close(loop, handle(() -> {
							dst.close(loop, handle(() -> {
								deleteFiles([path, newPath], () -> {
									print('Done');
									actions.next();
								});
							}));
						}));
					})(e));
				})(e));
			})(e));
		});
	}

	function access(actions:Actions) {
		var path = Misc.tmpDir();
		print('Checking write permissions on $path...');
		File.access(loop, path, [W_OK], handle(() -> {
			print('Done');
			actions.next();
		}));
	}

	function chmod(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-chmod';
		createFile(path, Bytes.ofString('123'), () -> {
			print('chmod on $path...');
			File.chmod(loop, path, 420, handle(() -> {
				deleteFiles([path], () -> {
					print('Done');
					actions.next();
				});
			}));
		});
	}

	function utime(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-utime';
		createFile(path, Bytes.ofString('123'), () -> {
			print('utime on $path...');
			File.utime(loop, path, Date.now().getTime(), Date.now().getTime(), handle(() -> {
				deleteFiles([path], () -> {
					print('Done');
					actions.next();
				});
			}));
		});
	}

	function linkSymlinkReadLinkRealPath(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-l';
		var newPath = Misc.tmpDir() + '/test-file-link';
		createFile(path, Bytes.ofString('123'), () -> {
			print('link $path to $newPath...');
			File.link(loop, path, newPath, handle(() -> {
				deleteFiles([newPath], () -> {
					print('symlink $path to $newPath...');
					File.symlink(loop, path, newPath, [SYMLINK_JUNCTION], handle(() -> {
						print('readlink at $newPath...');
						File.readLink(loop, newPath, (e, target) -> handle(() -> {
							print('Link content: $target');
							File.readLink(loop, newPath, (e, real) -> handle(() -> {
								print('Real path of $newPath: $real');
								deleteFiles([path, newPath], () -> {
									print('Done');
									actions.next();
								});
							})(e));
						})(e));
					}));
				});
			}));
		});
	}

	function chown(actions:Actions) {
		if(Sys.systemName() == 'Windows') {
			actions.next();
			return;
		}

		var path = Misc.tmpDir() + '/test-file-chown';
		createFile(path, Bytes.ofString(''), () -> {
			print('chown on $path...');
			File.chown(loop, path, -1, -1, handle(() -> {
				deleteFiles([path], () -> {
					print('Done');
					actions.next();
				});
			}));
		});
	}
}