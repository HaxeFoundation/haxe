import sys.io.File;

using StringTools;

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
		lines = lines.map(l -> l.trim().replace("\\", "/"));
		inline function check(module:String) {
			var matches = Lambda.filter(lines, l -> l.contains(module));

			if (matches.length == 0)
				throw 'Cannot find $module in dependency dump';

			for (line in matches) {
				if (line.endsWith(":")) line = line.substr(0, line.length - 1);
				if (!line.endsWith('tests/misc/projects/Issue11852/$module')) {
					trace(module, line);
					throw 'Incorrect path generated for $module';
				}
			}
		}

		// Check generated path for macro generated modules
		check("TopLevelType_1_1");
		check("foo/bar/DefinedType_1_2");
	}
	#end
}
