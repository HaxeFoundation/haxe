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

p1.ext == "ext";
p1.dir == "/dir1/dir2";
p1.file == "file";

p2.ext == null;
p2.dir == "/dir1/dir.with.dots";
p2.file == "file";

p3.ext == "htaccess";
p3.dir == null;
p3.file == "";

p4.ext == null;
p4.dir == "/dir";
p4.file == "";

p5.ext == null;
p5.dir == "..";
p5.file == "";

p6.ext == null;
p6.dir == ".";
p6.file == "";

// toString
p1.toString() == path;
p2.toString() == path2;
p3.toString() == path3;
p4.toString() == path4;

// withoutExtension
haxe.io.Path.withoutExtension(path) == "/dir1/dir2/file";
haxe.io.Path.withoutExtension(path2) == path2;
haxe.io.Path.withoutExtension(path3) == "";
haxe.io.Path.withoutExtension(path4) == "/dir/";

// withoutDirectory
haxe.io.Path.withoutDirectory(path) == "file.ext";
haxe.io.Path.withoutDirectory(path2) == "file";
haxe.io.Path.withoutDirectory(path3) == ".htaccess";
haxe.io.Path.withoutDirectory(path4) == "";

// directory
haxe.io.Path.directory(path) == "/dir1/dir2";
haxe.io.Path.directory(path2) == "/dir1/dir.with.dots";
haxe.io.Path.directory(path3) == "";
haxe.io.Path.directory(path4) == "/dir";

// extension
haxe.io.Path.extension(path) == "ext";
haxe.io.Path.extension(path2) == "";
haxe.io.Path.extension(path3) == "htaccess";
haxe.io.Path.extension(path4) == "";

// withExtension
haxe.io.Path.withExtension(path, "foo") == "/dir1/dir2/file.foo";
haxe.io.Path.withExtension(path2, "foo") == "/dir1/dir.with.dots\\file.foo";
haxe.io.Path.withExtension(path3, "foo") == ".foo";
haxe.io.Path.withExtension(path4, "foo") == "/dir/.foo";

// normalize
haxe.io.Path.normalize("dir1/dir2/../dir3") == "dir1/dir3";
haxe.io.Path.normalize("/dir1/dir2/../../test.foo") == "/test.foo";
haxe.io.Path.normalize("dir1/dir2/dir3/dir4/../../../dir5") == "dir1/dir5";
haxe.io.Path.normalize("C:\\Windows\\..\\Users/Waneck on Windows///.haxelib") == "C:/Users/Waneck on Windows/.haxelib";
haxe.io.Path.normalize("http://haxe.org/downloads") == "http://haxe.org/downloads";
haxe.io.Path.normalize("../mydir") == "../mydir";
haxe.io.Path.normalize("../../mydir") == "../../mydir";
haxe.io.Path.normalize("dir1/.././../mydir/..") == "..";
haxe.io.Path.normalize(".//dir1") == "dir1";
haxe.io.Path.normalize(".//.//dir1") == "dir1";
haxe.io.Path.normalize("././/dir1") == "dir1";
haxe.io.Path.normalize("././dir1") == "dir1";
haxe.io.Path.normalize("dir1/.//dir2") == "dir1/dir2";

// join
haxe.io.Path.join(["dir1/dir2", "dir3/dir4"]) == "dir1/dir2/dir3/dir4";
haxe.io.Path.join(["dir1/dir2/bad_dir/", "../dir3/dir4"]) == "dir1/dir2/dir3/dir4";
haxe.io.Path.join([]) == "";
haxe.io.Path.join(["dir1/dir2"]) == "dir1/dir2";
haxe.io.Path.join(["", "dir1"]) == "dir1";
haxe.io.Path.join(["dir1", ""]) == "dir1";
haxe.io.Path.join([null, "dir1"]) == "dir1";
haxe.io.Path.join(["dir1", null]) == "dir1";
haxe.io.Path.join([null]) == "";
haxe.io.Path.join([""]) == "";

// addTrailingSlash
haxe.io.Path.addTrailingSlash("") == "/";
haxe.io.Path.addTrailingSlash("a") == "a/";
haxe.io.Path.addTrailingSlash("a/") == "a/";
haxe.io.Path.addTrailingSlash("a/b") == "a/b/";
haxe.io.Path.addTrailingSlash("a/b/") == "a/b/";
haxe.io.Path.addTrailingSlash("a\\") == "a\\";
haxe.io.Path.addTrailingSlash("a\\b") == "a\\b\\";
haxe.io.Path.addTrailingSlash("a\\b\\") == "a\\b\\";

// isAbsolute
haxe.io.Path.isAbsolute("") == false;
haxe.io.Path.isAbsolute("some") == false;
haxe.io.Path.isAbsolute("some/other") == false;
haxe.io.Path.isAbsolute("/some") == true;
haxe.io.Path.isAbsolute("c:/some") == true;
