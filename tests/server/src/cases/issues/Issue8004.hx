package cases.issues;

import haxe.display.FsPath;
import haxe.display.Server;

class Issue8004 extends TestCase {
	@:variant("Js", "js", "test.js")
	@:variant("Jvm", "jvm", "test.jar")
	@:variant("Neko", "neko", "test.n")
	@:variant("Lua", "lua", "test.lua")
	@:variant("Python", "python", "test.py")
	@:variant("Swf", "swf", "test.swf")
	@:variant("Hashlink", "hl", "test.hl")
	@:variant("CPP", "cpp", "cpp")
	@:variant("PHP", "php", "php")
	@:variant("Eval", "--interp", null)
	function test(target:String, output:Null<String>) {
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = output == null ? ["-main", "Empty", target] : ["-main", "Empty", '-$target', 'bin/$output', "--no-output"];

		runHaxe(args);
		runHaxeJson(args, ServerMethods.ReadClassPaths, {wait: true});
		final result = runHaxeJson(args, DisplayMethods.WorkspaceSymbols, {filter: "uint"});

		var found = false;
		for (module in result) {
			for (symbol in module.symbols) {
				if (symbol.name == "UInt" && symbol.kind == Abstract) {
					found = true;
					break;
				}
			}
		}
		Assert.isTrue(found);
	}
}
