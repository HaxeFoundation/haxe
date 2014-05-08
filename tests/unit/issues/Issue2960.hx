package unit.issues;

class Issue2960 extends Test {
	static var map:Map<String, haxe.Constraints.Function>;

	static function call(m) {
		map = m;
	}

	function test() {
		call(["a" => function() {}]);
	}
}