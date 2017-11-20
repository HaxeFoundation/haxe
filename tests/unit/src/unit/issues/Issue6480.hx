package unit.issues;

class Issue6480 extends unit.Test {
	function test() {
		var o_blub = (function(f,o) {
			return function(increment) {
				return f(o,increment);
			};
		})(blub,{ counter : 0});
        eq(1, o_blub(true));
        eq(2, o_blub(true));
	}

    static function blub(o:{ counter: Int}, increment:Bool) {
        if (increment) o.counter++;
        else o.counter--;
        return o.counter;
    }
}