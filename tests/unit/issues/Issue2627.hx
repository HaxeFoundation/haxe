package unit.issues;

class Issue2627 extends unit.Test {

	var field:Int;

    function test1() {
        var c = 0;
        var s = staticAdd(c++, c++);
		eq(s, -1);
		eq(c, 2);
    }

	function test2() {
		field = 0;
		var s = memberAdd(field++, field++);
		eq(s, 1);
		eq(field, 2);
	}

	inline static function staticAdd(x:Float, y:Float) { return x - y; }

	inline function memberAdd(x:Float, y:Float) { return this.field - x - y; }
}