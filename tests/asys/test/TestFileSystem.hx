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
		OldFile.saveContent("resources-rw/access.txt", "");

		NewFS.chmod("resources-rw/access.txt", None);
		eq(NewFS.stat("resources-rw/access.txt").permissions, None);
		noExc(() -> NewFS.access("resources-rw/access.txt"));
		exc(() -> NewFS.access("resources-rw/access.txt", Read));

		NewFS.chmod("resources-rw/access.txt", "r-------x");
		eq(NewFS.stat("resources-rw/access.txt").permissions, "r-------x");
		noExc(() -> NewFS.access("resources-rw/access.txt", Read));
		exc(() -> NewFS.access("resources-rw/access.txt", Write));
		exc(() -> NewFS.access("resources-rw/access.txt", Execute));

		// cleanup
		OldFS.deleteFile("resources-rw/access.txt");
	}

	function testExists():Void {
		t(NewFS.exists("resources-ro/hello.txt"));
		t(NewFS.exists("resources-ro/binary.bin"));
		f(NewFS.exists("resources-ro/non-existent-file"));
	}

	function testMkdir():Void {
		// initially these directories don't exist
		f(OldFS.exists("resources-rw/mkdir"));
		f(OldFS.exists("resources-rw/mkdir/nested/dir"));

		// without `recursive`, this should not succeed
		exc(() -> NewFS.mkdir("resources-rw/mkdir/nested/dir"));

		// create a single directory
		NewFS.mkdir("resources-rw/mkdir");

		// create a directory recursively
		NewFS.mkdir("resources-rw/mkdir/nested/dir", true);

		t(OldFS.exists("resources-rw/mkdir"));
		t(OldFS.exists("resources-rw/mkdir/nested/dir"));
		f(OldFS.exists("resources-rw/mkdir/dir"));

		// raise if target already exists if not `recursive`
		exc(() -> NewFS.mkdir("resources-rw/mkdir/nested/dir"));

		// cleanup
		OldFS.deleteDirectory("resources-rw/mkdir/nested/dir");
		OldFS.deleteDirectory("resources-rw/mkdir/nested");
		OldFS.deleteDirectory("resources-rw/mkdir");
	}

	function testMkdtemp():Void {
		// empty `resources-rw` to begin with
		aeq(OldFS.readDirectory("resources-rw"), []);

		// create some temporary directories
		var dirs = [ for (i in 0...3) NewFS.mkdtemp("resources-rw/helloXXXXXX") ];

		for (f in OldFS.readDirectory("resources-rw")) {
			t(f.startsWith("hello"));
			t(OldFS.isDirectory('resources-rw/$f'));
			OldFS.deleteDirectory('resources-rw/$f');
		}

		// cleanup
		for (f in OldFS.readDirectory("resources-rw")) {
			OldFS.deleteDirectory('resources-rw/$f');
		}
	}

	function testReaddir():Void {
		aeq(NewFS.readdir("resources-rw"), []);
		aeq(NewFS.readdirTypes("resources-rw"), []);
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
		OldFile.saveContent("resources-rw/hello.txt", TestConstants.helloString);
		OldFile.saveContent("resources-rw/other.txt", "");
		OldFS.createDirectory("resources-rw/sub");
		OldFile.saveContent("resources-rw/sub/foo.txt", "");

		t(OldFS.exists("resources-rw/hello.txt"));
		f(OldFS.exists("resources-rw/world.txt"));

		// rename a file
		NewFS.rename("resources-rw/hello.txt", "resources-rw/world.txt");

		f(OldFS.exists("resources-rw/hello.txt"));
		t(OldFS.exists("resources-rw/world.txt"));
		eq(OldFile.getContent("resources-rw/world.txt"), TestConstants.helloString);

		// raises if the old path is non-existent
		exc(() -> NewFS.rename("resources-rw/non-existent", "resources-rw/foobar"));

		// raises if renaming file to directory
		exc(() -> NewFS.rename("resources-rw/world.txt", "resources-rw/sub"));

		// raises if renaming directory to file
		exc(() -> NewFS.rename("resources-rw/sub", "resources-rw/world.txt"));

		// rename a directory
		NewFS.rename("resources-rw/sub", "resources-rw/resub");

		f(OldFS.exists("resources-rw/sub"));
		t(OldFS.exists("resources-rw/resub"));
		aeq(OldFS.readDirectory("resources-rw/resub"), ["foo.txt"]);

		// renaming to existing file overrides it
		NewFS.rename("resources-rw/world.txt", "resources-rw/other.txt");

		f(OldFS.exists("resources-rw/world.txt"));
		t(OldFS.exists("resources-rw/other.txt"));
		eq(OldFile.getContent("resources-rw/other.txt"), TestConstants.helloString);

		// cleanup
		OldFS.deleteFile("resources-rw/other.txt");
		OldFS.deleteFile("resources-rw/resub/foo.txt");
		OldFS.deleteDirectory("resources-rw/resub");
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

		NewFS.createDirectory("resources-rw/foo");
		t(OldFS.exists("resources-rw/foo"));
		t(OldFS.isDirectory("resources-rw/foo"));
		NewFS.deleteDirectory("resources-rw/foo");
		f(OldFS.exists("resources-rw/foo"));
	}
	*/
}
