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

	@:coroutine function probe(defines:Array<String>, label:String) {
		vfs.putContent("A.hx", V1);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();

		var offset = V1.indexOf("aaa") + 1; // == V2.indexOf("bbb") + 1

		// NO invalidate: the compile above cached A's disk parse; hover sends newer unsaved contents.
		var res = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("A.hx"), offset: offset, contents: V2});
		var t = if (res == null || res.item == null) "<null>" else try res.item.type.args.path.typeName catch (_) "<untyped>";
		Assert.equals("String", t, label + ": got " + t);
	}

	function testBaseline() probe([], "baseline");
	function testResident() probe(["-D", "hxb.resident_modules"], "resident");
	function testHeaderInvalidation() probe(["-D", "hxb.header_invalidation"], "header_inv");
	function testBoth() probe(["-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"], "both");
}
