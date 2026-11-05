class Macro {
	static function init() {
		haxe.macro.Compiler.removeField("Main", "x");
	}
}
