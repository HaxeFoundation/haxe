package cases.issues;

import haxe.Json;
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
		runHaxeJson(args, ServerMethods.ReadClassPaths, null);
		runHaxe(args.concat(["--display", "?@0@workspace-symbols@uint"]));

		var result:Array<SymbolReply> = Json.parse(lastResult.stderr);
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

// From Haxe LSP; should be moved to haxe.display package when workspace symbols
// are added as Json RPC
private enum abstract ModuleSymbolKind(Int) {
	final Class = 1;
	final Interface;
	final Enum;
	final TypeAlias;
	final Abstract;
	final Field;
	final Property;
	final Method;
	final Constructor;
	final Function;
	final Variable;
	final Struct;
	final EnumAbstract;
	final Operator;
	final EnumMember;
	final Constant;
	final Module;
}

private typedef ModuleSymbolEntry = {
	final name:String;
	final kind:ModuleSymbolKind;
	final range:Range;
	final ?containerName:String;
	final ?isDeprecated:Bool;
}

private typedef SymbolReply = {
	final file:FsPath;
	final symbols:Array<ModuleSymbolEntry>;
}

// From Haxe server protocol
private typedef Position = {
	var line:Int;
	var character:Int;
}

private typedef Range = {
	var start:Position;
	var end:Position;
}
