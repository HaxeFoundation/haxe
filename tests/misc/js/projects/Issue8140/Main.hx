class Main {
	static function main() {
		#if with_name
		//should print `Array` but prints `true`
		haxe.Timer.delay(() -> trace(Type.getClassName(Array)), 0);
		haxe.Timer.delay(() -> trace(Math), 0);
		#else
		haxe.Timer.delay(() -> Std.string({}), 0);
		#end
	}

	function new() {}
}
