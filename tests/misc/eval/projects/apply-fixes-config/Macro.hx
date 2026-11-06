import haxe.macro.Compiler;

class Macro {
	static function init() {
		var fix = Compiler.getConfiguration().fix;
		Sys.println(fix == null ? "null" : [
			fix.scope.join(","), Std.string(fix.diff), Std.string(fix.passes), Std.string(fix.compat), Std.string(fix.allowDirty),
			Std.string(fix.unsafe), fix.only.join(",")
		].join(" "));
	}
}
