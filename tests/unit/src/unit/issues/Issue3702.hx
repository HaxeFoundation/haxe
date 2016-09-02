package unit.issues;

class Issue3702 extends Test {

	static var state = 0;

	function test() {
        var m = ["hello" => 0];
		eq(state, 0);
        var x = m[getKey()]++;
		eq(0, x);
		eq(1, state);
		eq(1, m["hello"]);
        var x = ++m[getKey()];
		eq(2, x);
		eq(2, state);
		eq(2, m["hello"]);
        var x = m[getKey()]--;
		eq(2, x);
		eq(3, state);
		eq(1, m["hello"]);
        var x = --m[getKey()];
		eq(0, x);
		eq(4, state);
		eq(0, m["hello"]);
	}

	static function getKey() {
		++state;
		return "hello";
	}
}