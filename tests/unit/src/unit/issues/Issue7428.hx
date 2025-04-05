package unit.issues;

class Issue7428 extends unit.Test {
	function test() {
		eq(null, getIntOrNull());
	}

	@:pure(false)
	static function getIntOrNull():Null<Int> {
		return execute(function():Null<Int> {
			return null;
		});
	}

	@:pure(false)
	static function execute<T>(callback:()->T):T {
		var result = callback();
		return result;
	}
}