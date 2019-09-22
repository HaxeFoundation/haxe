package test;

import haxe.io.Bytes;
import asys.FileSystem as NewFS;
import asys.io.File as NewFile;
import sys.FileSystem as OldFS;
import sys.io.File as OldFile;

using StringTools;

class TestFileSystem extends Test {
	/**
		Tests `FileSystem.access`, `perm` from `FileSystem.stat`, and
		`FileSystem.chmod`.
	**/
	function testAccess():Void {
		// create a file
		OldFile.saveContent('$testDir/access.txt', "");

		NewFS.chmod('$testDir/access.txt', None);

		if (Sys.systemName() == "Windows") {
			// Windows only allows distinguishing readonly
			eq(NewFS.stat('$testDir/access.txt').permissions, ReadOwner | ReadGroup | ReadOthers);
			exc(() -> NewFS.access('$testDir/access.txt', Write));

			NewFS.chmod('$testDir/access.txt', "r-------x");
			eq(NewFS.stat('$testDir/access.txt').permissions, ReadOwner | ReadGroup | ReadOthers);
			exc(() -> NewFS.access('$testDir/access.txt', Write));
		} else {
			eq(NewFS.stat('$testDir/access.txt').permissions, None);
			noExc(() -> NewFS.access('$testDir/access.txt'));
			exc(() -> NewFS.access('$testDir/access.txt', Read));

			NewFS.chmod('$testDir/access.txt', "r-------x");
			eq(NewFS.stat('$testDir/access.txt').permissions, "r-------x");
			noExc(() -> NewFS.access('$testDir/access.txt', Read));
			exc(() -> NewFS.access('$testDir/access.txt', Write));
			exc(() -> NewFS.access('$testDir/access.txt', Execute));
		}

		// cleanup
		NewFS.chmod('$testDir/access.txt', "rw------x");
		OldFS.deleteFile('$testDir/access.txt');
	}

	function testExists():Void {
		t(NewFS.exists("resources-ro/hello.txt"));
		t(NewFS.exists("resources-ro/binary.bin"));
		f(NewFS.exists("resources-ro/non-existent-file"));
	}

	function testMkdir():Void {
		// initially these directories don't exist
		f(OldFS.exists('$testDir/mkdir'));
		f(OldFS.exists('$testDir/mkdir/nested/dir'));

		// without `recursive`, this should not succeed
		exc(() -> NewFS.mkdir('$testDir/mkdir/nested/dir'));

		// create a single directory
		NewFS.mkdir('$testDir/mkdir');

		// create a directory recursively
		NewFS.mkdir('$testDir/mkdir/nested/dir', true);

		t(OldFS.exists('$testDir/mkdir'));
		t(OldFS.exists('$testDir/mkdir/nested/dir'));
		f(OldFS.exists('$testDir/mkdir/dir'));

		// raise if target already exists if not `recursive`
		exc(() -> NewFS.mkdir('$testDir/mkdir/nested/dir'));

		// cleanup
		OldFS.deleteDirectory('$testDir/mkdir/nested/dir');
		OldFS.deleteDirectory('$testDir/mkdir/nested');
		OldFS.deleteDirectory('$testDir/mkdir');
	}

	function testMkdtemp():Void {
		// empty `resources-rw` to begin with
		aeq(OldFS.readDirectory(testDir), []);

		// create some temporary directories
		var dirs = [ for (i in 0...3) NewFS.mkdtemp('$testDir/helloXXXXXX') ];

		for (f in OldFS.readDirectory(testDir)) {
			t(f.startsWith("hello"));
			t(OldFS.isDirectory('$testDir/$f'));
			OldFS.deleteDirectory('$testDir/$f');
		}

		// cleanup
		for (f in OldFS.readDirectory(testDir)) {
			OldFS.deleteDirectory('$testDir/$f');
		}
	}

	function testReaddir():Void {
		aeq(NewFS.readdir(testDir), []);
		aeq(NewFS.readdirTypes(testDir), []);
		aeq(NewFS.readdir("resources-ro"), ["binary.bin", "hello.txt"]);
		var res = NewFS.readdirTypes("resources-ro");
		eq(res.length, 2);
		eq(res[0].name, "binary.bin");
		eq(res[0].isBlockDevice(), false);
		eq(res[0].isCharacterDevice(), false);
		eq(res[0].isDirectory(), false);
		eq(res[0].isFIFO(), false);
		eq(res[0].isFile(), true);
		eq(res[0].isSocket(), false);
		eq(res[0].isSymbolicLink(), false);

		// raises if target is not a directory or does not exist
		exc(() -> NewFS.readdir("resources-ro/hello.txt"));
		exc(() -> NewFS.readdir("resources-ro/non-existent-directory"));
	}

	function testRename():Void {
		// setup
		OldFile.saveContent('$testDir/hello.txt', TestConstants.helloString);
		OldFile.saveContent('$testDir/other.txt', "");
		OldFS.createDirectory('$testDir/sub');
		OldFile.saveContent('$testDir/sub/foo.txt', "");

		t(OldFS.exists('$testDir/hello.txt'));
		f(OldFS.exists('$testDir/world.txt'));

		// rename a file
		NewFS.rename('$testDir/hello.txt', '$testDir/world.txt');

		f(OldFS.exists('$testDir/hello.txt'));
		t(OldFS.exists('$testDir/world.txt'));
		eq(OldFile.getContent('$testDir/world.txt'), TestConstants.helloString);

		// raises if the old path is non-existent
		exc(() -> NewFS.rename('$testDir/non-existent', '$testDir/foobar'));

		// raises if renaming file to directory
		exc(() -> NewFS.rename('$testDir/world.txt', '$testDir/sub'));

		// raises if renaming directory to file
		// exc(() -> NewFS.rename('$testDir/sub', '$testDir/world.txt'));

		// rename a directory
		NewFS.rename('$testDir/sub', '$testDir/resub');

		f(OldFS.exists('$testDir/sub'));
		t(OldFS.exists('$testDir/resub'));
		aeq(OldFS.readDirectory('$testDir/resub'), ["foo.txt"]);

		// renaming to existing file overrides it
		NewFS.rename('$testDir/world.txt', '$testDir/other.txt');

		f(OldFS.exists('$testDir/world.txt'));
		t(OldFS.exists('$testDir/other.txt'));
		eq(OldFile.getContent('$testDir/other.txt'), TestConstants.helloString);

		// cleanup
		OldFS.deleteFile('$testDir/other.txt');
		OldFS.deleteFile('$testDir/resub/foo.txt');
		OldFS.deleteDirectory('$testDir/resub');
	}

	function testStat():Void {
		var stat = NewFS.stat("resources-ro");
		t(stat.isDirectory());

		var stat = NewFS.stat("resources-ro/hello.txt");
		eq(stat.size, TestConstants.helloBytes.length);
		t(stat.isFile());

		var stat = NewFS.stat("resources-ro/binary.bin");
		eq(stat.size, TestConstants.binaryBytes.length);
		t(stat.isFile());

		var file = NewFS.open("resources-ro/binary.bin");
		var stat = file.stat();
		eq(stat.size, TestConstants.binaryBytes.length);
		t(stat.isFile());
		file.close();

		exc(() -> NewFS.stat("resources-ro/non-existent-file"));
	}

	/**
		Tests old filesystem APIs.
		`exists` is tested in `testExists`.
	**/
	/*
	function testCompat():Void {
		eq(NewFS.readFile("resources-ro/hello.txt").toString(), TestConstants.helloString);
		beq(NewFS.readFile("resources-ro/hello.txt"), TestConstants.helloBytes);
		beq(NewFS.readFile("resources-ro/binary.bin"), TestConstants.binaryBytes);
		t(NewFS.isDirectory("resources-ro"));
		f(NewFS.isDirectory("resources-ro/hello.txt"));
		aeq(NewFS.readDirectory("resources-ro"), ["binary.bin", "hello.txt"]);

		NewFS.createDirectory('$testDir/foo');
		t(OldFS.exists('$testDir/foo'));
		t(OldFS.isDirectory('$testDir/foo'));
		NewFS.deleteDirectory('$testDir/foo');
		f(OldFS.exists('$testDir/foo'));
	}
	*/
}
