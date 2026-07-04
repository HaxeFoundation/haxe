package cases;

import haxe.display.FsPath;
import haxe.display.Server;
import TestCase;
import utest.Assert;

// Regression coverage (2026-07-04 field report, not reproduced): body-only edit in A shifts positions; hover in A itself at the new offset must resolve
// against the NEW content. Offset chosen so v1 and v2 hold different identifiers with different
// types at the same byte offset (aaa:Int in v1, bbb:String in v2).
class HoverStalePositions extends TestCase {
	static inline var V1 = "class A {\n\tstatic function main() {\n\t\tvar aaa = 42;\n\t\ttrace(aaa);\n\t}\n}";
	static inline var V2 = "class A {\n\tstatic function main() {\n\t\tvar bbb = \"s\";\n\t\tvar aaa = 42;\n\t\ttrace(aaa);\n\t}\n}";
	static inline var V3 = "class A {\n\tstatic function main() {\n\t\tvar ccc = 1.5;\n\t\tvar bbb = \"s\";\n\t\tvar aaa = 42;\n\t\ttrace(aaa);\n\t}\n}";

	@:coroutine function hoverType(offset:Int):String {
		var res = runHaxeJson([], DisplayMethods.Hover, {file: new FsPath("A.hx"), offset: offset});
		if (res == null || res.item == null) return "<null>";
		return try res.item.type.args.path.typeName catch (_) "<untyped>";
	}

	@:coroutine function probe(defines:Array<String>, label:String) {
		vfs.putContent("A.hx", V1);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();

		// same byte offset: "aaa" in V1, "bbb" in V2 (line 2, after "\t\tvar ")
		var offset = V1.indexOf("aaa") + 1;
		Assert.equals(offset, V2.indexOf("bbb") + 1);

		// warm hover on v1 (Int)
		Assert.equals("Int", hoverType(offset), '$label: v1 hover');

		vfs.putContent("A.hx", V2);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});

		Assert.equals("String", hoverType(offset), '$label: v2 hover after invalidate');

		// and once more after a compile refresh
		runHaxe(args);
		assertSuccess();
		Assert.equals("String", hoverType(offset), '$label: v2 hover post-compile');
	}

	// Unsaved-buffer variant: disk keeps v1, hover passes v2 via the request's `contents`
	// (what vshaxe does on didChange before save), with the client-side invalidate that
	// haxe-language-server sends alongside.
	@:coroutine function probeUnsaved(defines:Array<String>, label:String) {
		vfs.putContent("A.hx", V1);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();

		var offset = V1.indexOf("aaa") + 1;

		Assert.equals("Int", hoverType(offset), '$label: v1 hover');

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});
		var res = runHaxeJson([], DisplayMethods.Hover, {file: new FsPath("A.hx"), offset: offset, contents: V2});
		var t = if (res == null || res.item == null) "<null>" else try res.item.type.args.path.typeName catch (_) "<untyped>";
		Assert.equals("String", t, '$label: v2 unsaved hover');

		// Second unsaved hover with evolved contents, NO invalidate between (mid-typing race):
		// the previous request's contents-parse must not be served for the new offsets.
		var offset3 = V3.indexOf("ccc") + 1;
		res = runHaxeJson([], DisplayMethods.Hover, {file: new FsPath("A.hx"), offset: offset3, contents: V3});
		t = if (res == null || res.item == null) "<null>" else try res.item.type.args.path.typeName catch (_) "<untyped>";
		Assert.equals("Float", t, '$label: v3 unsaved hover no-invalidate');
	}

	function testBaseline() probe([], "baseline");
	function testResident() probe(["-D", "hxb.resident_modules"], "resident");
	function testHeaderInvalidation() probe(["-D", "hxb.header_invalidation"], "header_inv");
	function testBoth() probe(["-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"], "both");

	function testUnsavedBaseline() probeUnsaved([], "unsaved-baseline");
	function testUnsavedResident() probeUnsaved(["-D", "hxb.resident_modules"], "unsaved-resident");
	function testUnsavedHeaderInvalidation() probeUnsaved(["-D", "hxb.header_invalidation"], "unsaved-header_inv");
	function testUnsavedBoth() probeUnsaved(["-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"], "unsaved-both");
}
