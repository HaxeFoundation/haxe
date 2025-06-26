import haxe.macro.Context;

class Main {
	static function main() {
		test();
	}

	macro static public function test() {
		try {
			Context.getType("");
		} catch(e:Dynamic) {
			Context.error(Std.string(e), Context.currentPos());
		}
		return macro {};
	}
}