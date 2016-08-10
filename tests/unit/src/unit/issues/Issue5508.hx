package unit.issues;

class Issue5508 extends unit.Test {
    var i = 2;

	function test() {
		eq(~mask(i), -5);
	}

    @:pure
    inline static function mask(i:Int):Int {
        return 0x1 << i;
    }
}
