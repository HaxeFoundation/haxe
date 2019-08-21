package cases;

class Issue7051 extends DisplayTestCase {
	/**
		class CWithCtor {
			public function new() { }
		}
		class CInheritedCtor extends CWithCtor { }
		class CInheritedCtor2 extends CInheritedCtor { }
		class CNoCtor { }
		class CNoCtor2 extends CNoCtor { }

		abstract AWithCtor(String) {
			public function new() this = "";
		}
		abstract AWithoutCtor(String) { }

		class Main {
			static function main() {
				new {-1-}
			}
		}
	**/
	function test() {
		var toplevel = toplevel(pos(1));
		eq(true, hasToplevel(toplevel, "type", "CWithCtor"));
		eq(true, hasToplevel(toplevel, "type", "CInheritedCtor"));
		eq(true, hasToplevel(toplevel, "type", "CInheritedCtor2"));
		eq(false, hasToplevel(toplevel, "type", "CNoCtor"));
		eq(false, hasToplevel(toplevel, "type", "CNoCtor2"));

		eq(true, hasToplevel(toplevel, "type", "AWithCtor"));
		eq(false, hasToplevel(toplevel, "type", "AWithoutCtor"));
	}

	/**
		new {-1-}
		call();
	**/
	@:funcCode function testBroken() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "Array"));
	}
}
