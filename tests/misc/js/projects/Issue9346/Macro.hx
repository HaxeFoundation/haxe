import haxe.macro.Compiler;
import haxe.macro.Context;

class Macro {
	static function init() {
		Compiler.addClassPath(Context.resolvePath("mystd"));
	}
}
