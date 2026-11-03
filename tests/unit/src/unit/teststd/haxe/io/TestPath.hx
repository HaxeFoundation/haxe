package unit.teststd.haxe.io;

class TestPath extends unit.Test {
	public function test() {
		var path = "/dir1/dir2/file.ext";
		var path2 = "/dir1/dir.with.dots\\file";
		var path3 = ".htaccess";
		var path4 = "/dir/";
		var path5 = "..";
		var path6 = ".";

		var p1 = new haxe.io.Path(path);
		var p2 = new haxe.io.Path(path2);
		var p3 = new haxe.io.Path(path3);
		var p4 = new haxe.io.Path(path4);
		var p5 = new haxe.io.Path(path5);
		var p6 = new haxe.io.Path(path6);

		eq(p1.ext, "ext");
		eq(p1.dir, "/dir1/dir2");
		eq(p1.file, "file");

		eq(p2.ext, null);
		eq(p2.dir, "/dir1/dir.with.dots");
		eq(p2.file, "file");

		eq(p3.ext, "htaccess");
		eq(p3.dir, null);
		eq(p3.file, "");

		eq(p4.ext, null);
		eq(p4.dir, "/dir");
		eq(p4.file, "");

		eq(p5.ext, null);
		eq(p5.dir, "..");
		eq(p5.file, "");

		eq(p6.ext, null);
		eq(p6.dir, ".");
		eq(p6.file, "");

		// toString
		eq(p1.toString(), path);
		eq(p2.toString(), path2);
		eq(p3.toString(), path3);
		eq(p4.toString(), path4);

		// withoutExtension
		eq(haxe.io.Path.withoutExtension(path), "/dir1/dir2/file");
		eq(haxe.io.Path.withoutExtension(path2), path2);
		eq(haxe.io.Path.withoutExtension(path3), "");
		eq(haxe.io.Path.withoutExtension(path4), "/dir/");

		// withoutDirectory
		eq(haxe.io.Path.withoutDirectory(path), "file.ext");
		eq(haxe.io.Path.withoutDirectory(path2), "file");
		eq(haxe.io.Path.withoutDirectory(path3), ".htaccess");
		eq(haxe.io.Path.withoutDirectory(path4), "");

		// directory
		eq(haxe.io.Path.directory(path), "/dir1/dir2");
		eq(haxe.io.Path.directory(path2), "/dir1/dir.with.dots");
		eq(haxe.io.Path.directory(path3), "");
		eq(haxe.io.Path.directory(path4), "/dir");

		// extension
		eq(haxe.io.Path.extension(path), "ext");
		eq(haxe.io.Path.extension(path2), "");
		eq(haxe.io.Path.extension(path3), "htaccess");
		eq(haxe.io.Path.extension(path4), "");

		// withExtension
		eq(haxe.io.Path.withExtension(path, "foo"), "/dir1/dir2/file.foo");
		eq(haxe.io.Path.withExtension(path2, "foo"), "/dir1/dir.with.dots\\file.foo");
		eq(haxe.io.Path.withExtension(path3, "foo"), ".foo");
		eq(haxe.io.Path.withExtension(path4, "foo"), "/dir/.foo");

		// normalize
		eq(haxe.io.Path.normalize("dir1/dir2/../dir3"), "dir1/dir3");
		eq(haxe.io.Path.normalize("/dir1/dir2/../../test.foo"), "/test.foo");
		eq(haxe.io.Path.normalize("dir1/dir2/dir3/dir4/../../../dir5"), "dir1/dir5");
		eq(haxe.io.Path.normalize("C:\\Windows\\..\\Users/Waneck on Windows///.haxelib"), "C:/Users/Waneck on Windows/.haxelib");
		eq(haxe.io.Path.normalize("http://haxe.org/downloads"), "http://haxe.org/downloads");
		eq(haxe.io.Path.normalize("../mydir"), "../mydir");
		eq(haxe.io.Path.normalize("../../mydir"), "../../mydir");
		eq(haxe.io.Path.normalize("dir1/.././../mydir/.."), "..");
		eq(haxe.io.Path.normalize(".//dir1"), "dir1");
		eq(haxe.io.Path.normalize(".//.//dir1"), "dir1");
		eq(haxe.io.Path.normalize("././/dir1"), "dir1");
		eq(haxe.io.Path.normalize("././dir1"), "dir1");
		eq(haxe.io.Path.normalize("dir1/.//dir2"), "dir1/dir2");

		// join
		eq(haxe.io.Path.join(["dir1/dir2", "dir3/dir4"]), "dir1/dir2/dir3/dir4");
		eq(haxe.io.Path.join(["dir1/dir2/bad_dir/", "../dir3/dir4"]), "dir1/dir2/dir3/dir4");
		eq(haxe.io.Path.join([]), "");
		eq(haxe.io.Path.join(["dir1/dir2"]), "dir1/dir2");
		eq(haxe.io.Path.join(["", "dir1"]), "dir1");
		eq(haxe.io.Path.join(["dir1", ""]), "dir1");
		eq(haxe.io.Path.join([null, "dir1"]), "dir1");
		eq(haxe.io.Path.join(["dir1", null]), "dir1");
		eq(haxe.io.Path.join([null]), "");
		eq(haxe.io.Path.join([""]), "");

		// addTrailingSlash
		eq(haxe.io.Path.addTrailingSlash(""), "/");
		eq(haxe.io.Path.addTrailingSlash("a"), "a/");
		eq(haxe.io.Path.addTrailingSlash("a/"), "a/");
		eq(haxe.io.Path.addTrailingSlash("a/b"), "a/b/");
		eq(haxe.io.Path.addTrailingSlash("a/b/"), "a/b/");
		eq(haxe.io.Path.addTrailingSlash("a\\"), "a\\");
		eq(haxe.io.Path.addTrailingSlash("a\\b"), "a\\b\\");
		eq(haxe.io.Path.addTrailingSlash("a\\b\\"), "a\\b\\");

		// isAbsolute
		f(haxe.io.Path.isAbsolute(""));
		f(haxe.io.Path.isAbsolute("some"));
		f(haxe.io.Path.isAbsolute("some/other"));
		t(haxe.io.Path.isAbsolute("/some"));
		t(haxe.io.Path.isAbsolute("c:/some"));

	}
}
