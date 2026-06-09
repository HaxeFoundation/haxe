class Main {
	static function main() {
		new Main();
	}
	public function new() {
		tween(this, {
			tint: Red // Unknown identifier : Red
		});
	}
	extern inline overload function tween(obj:Main, config:{?tint:Color}):Void trace("1");
	extern inline overload function tween(obj:Dynamic, config:{}):Void trace("2");
}
