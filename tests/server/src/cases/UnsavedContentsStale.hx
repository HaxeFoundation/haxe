package cases;

import haxe.display.FsPath;
import haxe.display.Server;
import TestCase;
import utest.Assert;

// A hover carrying unsaved buffer contents must be answered against THOSE contents even when the
// server has a cached parse of the file whose disk mtime still matches (file not saved). Models
// vshaxe: edit -> (invalidate deduplicated away / compile re-cached the disk parse) -> hover.
class UnsavedContentsStale extends TestCase {
	static inline var V1 = "class A {\n\tstatic function main() {\n\t\tvar aaa = 42;\n\t\ttrace(aaa);\n\t}\n}";
	static inline var V2 = "class A {\n\tstatic function main() {\n\t\tvar bbb = \"s\";\n\t\tvar aaa = 42;\n\t\ttrace(aaa);\n\t}\n}";

	function test(_) {
		vfs.putContent("A.hx", V1);
		var args = ["-main", "A", "-js", "no.js", "--no-output"];
		runHaxe(args);
		assertSuccess();

		var offset = V1.indexOf("aaa") + 1; // == V2.indexOf("bbb") + 1

		// NO invalidate: the compile above cached A's disk parse; hover sends newer unsaved contents.
		var res = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("A.hx"), offset: offset, contents: V2});
		var t = if (res == null || res.item == null) "<null>" else try res.item.type.args.path.typeName catch (_) "<untyped>";
		Assert.equals("String", t, "got " + t);
	}

	static inline var DISK = "class A { static function main() { Sys.println(\"from-disk\"); } }";
	static inline var BUFFER = "class A { static function main() { Sys.println(\"from-buffer\"); } }";

	// Diagnostics with unsaved contents populates the module cache. A later compilation without
	// contents must not reuse the modules typed from the buffer.
	function testDiagnosticsThenCompile(_) {
		vfs.putContent("A.hx", DISK);
		var args = ["-main", "A", "--interp"];
		runHaxe(args);
		assertHasPrint("from-disk");
		runHaxeJson(args, DisplayMethods.Diagnostics, {fileContents: [{file: new FsPath("A.hx"), contents: BUFFER}]});
		runHaxe(args);
		assertHasPrint("from-disk");
		Assert.isFalse(hasMessage("Haxe print: from-buffer"), "buffer contents leaked into compilation");
	}

	function testColdDiagnosticsThenCompile(_) {
		vfs.putContent("A.hx", DISK);
		var args = ["-main", "A", "--interp"];
		runHaxeJson(args, DisplayMethods.Diagnostics, {fileContents: [{file: new FsPath("A.hx"), contents: BUFFER}]});
		runHaxe(args);
		assertHasPrint("from-disk");
		Assert.isFalse(hasMessage("Haxe print: from-buffer"), "buffer contents leaked into compilation");
	}
}
