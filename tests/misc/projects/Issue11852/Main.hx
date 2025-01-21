import sys.io.File;

class Main {
	#if macro
	public static function init() {
		var pos = haxe.macro.Context.currentPos();

		haxe.macro.Context.onAfterInitMacros(() -> {
			haxe.macro.Context.defineType({
				pack: [],
				name: "TopLevelType",
				pos: pos,
				kind: TDStructure,
				fields: []
			});

			haxe.macro.Context.defineType({
				pack: ["foo", "bar"],
				name: "DefinedType",
				pos: pos,
				kind: TDStructure,
				fields: []
			});
		});
	}
	#else
	public static function main() {
		// Add generated modules as dependencies for Main
		var _:TopLevelType = {};
		var _:foo.bar.DefinedType = {};

		var lines = File.getContent("dump/eval/dependencies.dump").split("\n");
		lines = lines.map(l -> StringTools.replace(l, "\\", "/"));
		inline function check(module:String) {
			var line = Lambda.filter(lines, l -> StringTools.endsWith(l, module)).shift();

			if (line == null)
				throw 'Cannot find $module in dependency dump';

			if (!StringTools.endsWith(line, 'tests/misc/projects/Issue11852/$module')) {
				trace(module, line);
				throw 'Incorrect path generated for $module';
			}
		}

		// Check generated path for macro generated modules
		check("TopLevelType_1_1");
		check("foo/bar/DefinedType_1_2");
	}
	#end
}
