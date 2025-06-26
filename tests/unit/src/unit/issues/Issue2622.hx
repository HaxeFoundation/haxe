package unit.issues;
import unit.Test;

private abstract A(Int) from Int to Int {
	@:to public function toString():String {
		return Std.string(this);
	}
}

class Issue2622 extends Test {
	function test() {
		var a:A = 2;
		var b = foo(a, function(v) return v);
		eq("2", b);

		bar(function() return 1); // allow assigning any return type to Void
	}

	static function foo(v:A, test:A->String) {
		return test(v);
	}

	static function bar(test:()->Void) {

	}
}