package unit.issues;

class Issue7531 extends unit.Test {
	function test() {
		var kv = get();
		eq(0, kv.key);
		eq(1, kv.value);
	}

	static var arr = [1, 2, 3];
	static var idx = 0;

	static function get() {
		return {value:arr[idx], key:idx++};
	}
}