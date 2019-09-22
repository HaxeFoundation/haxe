package test;

import utest.Assert;
import asys.FileWatcherEvent;
import utest.Async;
import asys.FileSystem as NewFS;
import asys.io.File as NewFile;
import sys.FileSystem as OldFS;
import sys.io.File as OldFile;

class TestAsyncFileSystem extends Test {
	function testAsync(async:Async) {
		sub(async, done -> NewFS.async.exists("resources-ro/hello.txt", (error, exists) -> {
			t(exists);
			done();
		}));
		sub(async, done -> NewFS.async.exists("resources-ro/non-existent-file", (error, exists) -> {
			f(exists);
			done();
		}));
		sub(async, done -> NewFS.async.readdir("resources-ro", (error, names) -> {
			aeq(names, ["binary.bin", "hello.txt"]);
			done();
		}));

		eq(asyncDone, 0);
	}

	function testStat(async:Async) {
		sub(async, done -> {
			NewFS.async.stat("resources-ro", (error, stat) -> {
				eq(error, null);
				t(stat.isDirectory());
				done();
			});
		});

		sub(async, done -> {
			NewFS.async.stat("resources-ro/hello.txt", (error, stat) -> {
				eq(error, null);
				eq(stat.size, TestConstants.helloBytes.length);
				t(stat.isFile());
				done();
			});
		});

		sub(async, done -> {
			NewFS.async.stat("resources-ro/binary.bin", (error, stat) -> {
				eq(error, null);
				eq(stat.size, TestConstants.binaryBytes.length);
				t(stat.isFile());
				done();
			});
		});

		sub(async, done -> {
			var file = NewFS.open("resources-ro/binary.bin");
			file.async.stat((err, stat) -> {
				eq(err, null);
				eq(stat.size, TestConstants.binaryBytes.length);
				t(stat.isFile());
				file.close();
				done();
			});
		});

		sub(async, done -> {
			NewFS.async.stat("resources-ro/non-existent-file", (error, nd) -> {
				neq(error, null);
				eq(nd, null);
				done();
			});
		});

		eq(asyncDone, 0);
	}

	@:timeout(500)
	function testWatcher(async:Async) {
		var dir = '$testDir/watch';
		sys.FileSystem.createDirectory(dir);
		var expectedEvents:Array<FileWatcherEvent -> Void> = [
			event -> switch(event) {
				case Rename("foo"): Assert.pass();
				case _: Assert.fail("Expected Rename(foo) but got " + event);
			},
			event -> switch(event) {
				case Rename("foo/hello.txt" | "foo\\hello.txt"): Assert.pass();
				case _: Assert.fail("Expected Rename(foo/hello.txt) but got " + event);
			},
			event -> switch(event) {
				case Change("foo/hello.txt" | "foo\\hello.txt"): Assert.pass();
				case _: Assert.fail("Expected Change(foo/hello.txt) but got " + event);
			}
		];

		var watcher = NewFS.watch(dir, true);
		watcher.closeSignal.on(_ -> {
			async.done();
			OldFS.deleteDirectory(dir);
		});
		watcher.errorSignal.on(e -> assert('unexpected error: ${e.message}'));

		var continuations = [];

		watcher.changeSignal.on(event -> {
			t(expectedEvents.length > 0);
			var expected = expectedEvents.shift();
			expected(event);
			if (continuations.length > 0) {
				continuations.shift()();
			}
			if (expectedEvents.length == 0) {
				watcher.close();
			}
		});

		continuations.push(() -> {
			var file = NewFS.open('$dir/foo/hello.txt', "w");
			file.truncate(10);
			file.close();
		});
		continuations.push(() -> {
			var file = NewFS.open('$dir/foo/hello.txt', "w");
			file.truncate(5);
			file.close();
		});
		NewFS.mkdir('$dir/foo');
	}
}
