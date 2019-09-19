package test;

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
		TestBase.uvRun();
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
		TestBase.uvRun();
	}

	@:timeout(3000)
	function testWatcher(async:Async) {
		var dir = "resources-rw/watch";
		sys.FileSystem.createDirectory(dir);
		var events = [];

		var watcher = NewFS.watch(dir, true);
		watcher.closeSignal.on(_ -> {
			async.done();
			OldFS.deleteDirectory(dir);
		});
		watcher.errorSignal.on(e -> assert('unexpected error: ${e.message}'));
		watcher.changeSignal.on(events.push);

		NewFS.mkdir('$dir/foo');

		TestBase.uvRun(RunOnce);
		t(events.length == 1 && events[0].match(Rename("foo")));
		events.resize(0);

		var file = NewFS.open('$dir/foo/hello.txt', "w");
		file.truncate(10);
		file.close();
		NewFS.unlink('$dir/foo/hello.txt');

		NewFS.rmdir('$dir/foo');

		TestBase.uvRun(RunOnce);
		t(events.length == 2 && events[0].match(Rename("foo/hello.txt")));
		t(events.length == 2 && events[1].match(Rename("foo")));
		events.resize(0);

		watcher.close();
		TestBase.uvRun(RunOnce);
	}
}
