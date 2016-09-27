package unit.issues.misc;

class Issue3560Macro {
	macro static public function getImportString() {
		var imports = haxe.macro.Context.getLocalImports();
		var s = imports.map(function (i) {
			return Std.string(i.mode) + ":" + i.path.map(function(p) return p.name).join(".");
		});
		return macro $v{s};
	}
}