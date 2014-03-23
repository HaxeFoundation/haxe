package unit.issues;
import unit.Test;

class Issue2558 extends Test {
	function test() {
		feq(10, parse("1e+1"));
	}

	macro static function parse(s:String) {
		var f = Std.parseFloat(s);
		return macro $v{f};
	}
}