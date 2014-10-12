package unit.issues;
import unit.Test;


class Issue2696 extends Test {
    private static var value = 2;

    inline private static function func() :Int {
        return -value;
    }

	function test() {
		eq(2, -func());
		eq(2, value);
	}
}