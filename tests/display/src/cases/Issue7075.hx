package cases;

class Issue7075 extends DisplayTestCase {
	/**
	import hax{-1-}

	class Main {
		static function main() {}
	}
	**/
	function test1() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "CallStack"));
	}

	/**
	import haxe.d{-1-}

	class Main {
		static function main() {}
	}
	**/
	function test2() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "CallStack", ""));
	}
}