var path = "/dir1/dir2/file.ext";
var path2 = "/dir1/dir.with.dots\\file";
var path3 = ".htaccess";
var path4 = "/dir/";

var p1 = new haxe.io.Path(path);
var p2 = new haxe.io.Path(path2);
var p3 = new haxe.io.Path(path3);
var p4 = new haxe.io.Path(path4);

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

// addTrailingSlash
haxe.io.Path.addTrailingSlash("") == "/";
haxe.io.Path.addTrailingSlash("a") == "a/";
haxe.io.Path.addTrailingSlash("a/") == "a/";
haxe.io.Path.addTrailingSlash("a/b") == "a/b/";
haxe.io.Path.addTrailingSlash("a/b/") == "a/b/";
haxe.io.Path.addTrailingSlash("a\\") == "a\\";
haxe.io.Path.addTrailingSlash("a\\b") == "a\\b\\";
haxe.io.Path.addTrailingSlash("a\\b\\") == "a\\b\\";