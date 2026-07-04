package cases;

import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

// hxb.lazy_inheritance forwarding stubs vs TInst identity (field report: heaps
// `flow.getProperties(icon)` completed/hovered as TMono while compile/diagnostics
// resolved fine). Requires an argument whose class is a SUBCLASS of the parameter
// type, restored from hxb: the unify walk crossed an unbuilt stub (empty cl_super)
// and per-request stub generations never merged with resident/served real classes.
class LazyInheritanceForwarding extends TestCase {
	static var MAIN_V1 = "class Main {\n\tstatic function main() {\n\t\tvar flow = new h2d.Flow();\n\t\tvar o = new h2d.Bitmap();\n\t}\n}";
	static var MAIN_V2 = "class Main {\n\tstatic function main() {\n\t\tvar flow = new h2d.Flow();\n\t\tvar o = new h2d.Bitmap();\n\t\tvar props = flow.getProperties(o);\n\t\tprops.\n\t}\n}";

	function setupFiles() {
		vfs.putContent("Main.hx", MAIN_V1);
		vfs.putContent("h2d/Object.hx", "package h2d;\nclass Object {\n\tpublic function new() {}\n}");
		vfs.putContent("h2d/Drawable.hx", "package h2d;\nclass Drawable extends Object {}");
		vfs.putContent("h2d/Bitmap.hx", "package h2d;\nclass Bitmap extends Drawable {}");
		vfs.putContent("h2d/Flow.hx", "package h2d;\n\nclass FlowProperties {\n\tpublic var paddingLeft = 0;\n\tfunction new() {}\n}\n\nclass Flow extends Object {\n\tvar properties : Array<FlowProperties> = [];\n\tpublic function getProperties( e : h2d.Object ) {\n\t\treturn properties[0];\n\t}\n}");
	}

	@:coroutine function checkCompletion(args:Array<String>, label:String) {
		var offset = MAIN_V2.indexOf("props.\n") + "props.".length;
		var res = runHaxeJson(args, DisplayMethods.Completion,
			{file: new FsPath("Main.hx"), offset: offset, wasAutoTriggered: true, contents: MAIN_V2});
		var found = res != null && res.items.exists(item -> switch item.kind {
			case ClassField: item.args.field.name == "paddingLeft";
			case _: false;
		});
		Assert.isTrue(found, '$label: completion missing paddingLeft (${res == null ? -1 : res.items.length} items)');
	}

	@:coroutine function checkHover(args:Array<String>, label:String) {
		var offset = MAIN_V2.indexOf("var props") + "var pr".length;
		var res = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: offset, contents: MAIN_V2});
		var t = if (res == null || res.item == null) "<null>" else try res.item.type.args.path.typeName catch (_) "<" + res.item.type.kind + ">";
		Assert.equals("FlowProperties", t, '$label: hover got $t');
	}

	@:coroutine function probe(defines:Array<String>, label:String) {
		setupFiles();
		var args = ["-main", "Main", "-js", "no.js", "--no-output"].concat(defines);

		// warm hxb cache, then a display request on V1 restores Flow's signature (stubs)
		// without touching getProperties; the V2 completion then unifies Bitmap against
		// the restored parameter type
		runHaxe(args);
		assertSuccess();
		var hoverFlowOffset = MAIN_V1.indexOf("var flow") + "var fl".length;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: hoverFlowOffset, contents: MAIN_V1});
		checkCompletion(args, '$label/afterHover');
		checkHover(args, '$label/hover');

		// edited dependency: invalidate Flow.hx, then display requests only
		runHaxe(args);
		assertSuccess();
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("h2d/Flow.hx")});
		checkCompletion(args, '$label/postInvalidate');
		checkHover(args, '$label/postInvalidateHover');
	}

	function testLazyInheritance(_) probe(["-D", "hxb.lazy_inheritance"], "lazy_inh");

	function testAllLevers(_) probe([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "all_levers");
}
