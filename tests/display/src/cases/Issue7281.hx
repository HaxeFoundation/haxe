package cases;

class Issue7281 extends DisplayTestCase {
	/**
		extern class PrivateExternConstructor {
			function new() { }
		}

		class PrivateConstructor {
			function new() { }

			static function test() {
				new {-1-}
			}
		}

		class Ext1 extends PrivateConstructor {
			static function test() {
				new {-2-}
			}
		}

		class Main {
			static function main() {
				new {-3-}
				@:privateAccess new {-4-}
			}
		}
	**/
	function test() {
		var items = toplevel(pos(1));
		eq(true, hasToplevel(items, "type", "PrivateConstructor"));
		eq(true, hasToplevel(items, "type", "PrivateExternConstructor"));

		var items = toplevel(pos(2));
		eq(true, hasToplevel(items, "type", "PrivateConstructor"));
		eq(true, hasToplevel(items, "type", "PrivateExternConstructor"));

		var items = toplevel(pos(3));
		eq(false, hasToplevel(items, "type", "PrivateConstructor"));
		eq(true, hasToplevel(items, "type", "PrivateExternConstructor"));

		var items = toplevel(pos(4));
		eq(true, hasToplevel(items, "type", "PrivateConstructor"));
		eq(true, hasToplevel(items, "type", "PrivateExternConstructor"));
	}
}
