class Main {
	static var tmp:Int;

	@:analyzer(ignore)
	static function main() {
		var finally = 999;
		tmp = finally;
	}

	#if macro
	static function setupGenerator() {
		haxe.macro.Compiler.setCustomJSGenerator(api -> new haxe.macro.ExampleJSGenerator(api).generate());
	}
	#end
}