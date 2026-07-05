package cases;

import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

// hxb.lazy_inheritance forwarding stubs vs inherited-field lookup (field report:
// `layer.setBaseEntries` hovered/completed as Dynamic in a class whose SUPER declares
// `layer`, while compile/diagnostics resolved fine). raw_class_field walks
// cl_fields/cl_super without forcing unbuilt stubs, so the inherited ident lookup
// dead-ends and display recovery types it as Dynamic.
class LazyInheritanceFieldLookup extends TestCase {
	static var MAIN_V1 = "class Main extends Proc {\n\tstatic function main() {\n\t\tnew Main();\n\t}\n\tfunction new() {\n\t\tsuper();\n\t}\n}";
	static var MAIN_COMPLETION = "class Main extends Proc {\n\tstatic function main() {\n\t\tnew Main();\n\t}\n\tfunction new() {\n\t\tsuper();\n\t}\n\tfunction updateNav() {\n\t\tlayer.\n\t}\n}";
	static var MAIN_HOVER = "class Main extends Proc {\n\tstatic function main() {\n\t\tnew Main();\n\t}\n\tfunction new() {\n\t\tsuper();\n\t}\n\tfunction updateNav() {\n\t\t$type(layer.setActiveEntries);\n\t}\n}";

	function setupFiles() {
		vfs.putContent("Main.hx", MAIN_V1);
		vfs.putContent("BaseProc.hx", "class BaseProc {\n\tpublic function new() {}\n}");
		vfs.putContent("Proc.hx", "class Proc extends BaseProc {\n\tpublic var layer:ControlLayer;\n\tpublic function new() {\n\t\tsuper();\n\t\tlayer = new ControlLayer();\n\t}\n}");
		vfs.putContent("ControlLayer.hx", "class ControlLayer {\n\tpublic function new() {}\n\tpublic function setBaseEntries(entries:Array<String>, ?skipUpdate:Bool):Void {}\n\tpublic function setActiveEntries(entries:Null<Array<String>>, ?skipUpdate:Bool):Void {}\n}");
	}

	@:coroutine function checkCompletion(args:Array<String>, label:String) {
		var offset = MAIN_COMPLETION.indexOf("layer.\n") + "layer.".length;
		var res = runHaxeJson(args, DisplayMethods.Completion,
			{file: new FsPath("Main.hx"), offset: offset, wasAutoTriggered: true, contents: MAIN_COMPLETION});
		var found = res != null && res.items.exists(item -> switch item.kind {
			case ClassField: item.args.field.name == "setBaseEntries";
			case _: false;
		});
		Assert.isTrue(found, '$label: completion missing setBaseEntries (${res == null ? -1 : res.items.length} items)');
	}

	@:coroutine function checkHover(args:Array<String>, label:String) {
		var offset = MAIN_HOVER.indexOf("setActiveEntries") + "setActive".length;
		var res = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: offset, contents: MAIN_HOVER});
		var kind = if (res == null || res.item == null) "<null>" else Std.string(res.item.type.kind);
		Assert.equals("TFun", kind, '$label: hover got $kind');
	}

	@:coroutine function probe(defines:Array<String>, label:String) {
		setupFiles();
		var args = ["-main", "Main", "-js", "no.js", "--no-output"].concat(defines);

		// warm hxb cache, then a display request on V1 restores the super chain as
		// stubs; the V2 requests then resolve inherited `layer` through those stubs
		runHaxe(args);
		assertSuccess();
		var hoverOffset = MAIN_V1.indexOf("new Main") + "new Ma".length;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: hoverOffset, contents: MAIN_V1});
		checkCompletion(args, '$label/afterHover');
		checkHover(args, '$label/hover');

		// edited dependency: invalidate Proc.hx, then display requests only
		runHaxe(args);
		assertSuccess();
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Proc.hx")});
		checkCompletion(args, '$label/postInvalidate');
		checkHover(args, '$label/postInvalidateHover');
	}

	function testLazyInheritance(_) probe(["-D", "hxb.lazy_inheritance"], "lazy_inh");

	function testAllLevers(_) probe([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "all_levers");
}
