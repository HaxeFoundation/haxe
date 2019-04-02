package cases;

class Issue8014 extends DisplayTestCase {
	/**
		class Main extends MainLoo{-1-} {
			static function main() {}
		}
	**/
	function testExtends() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "type", "MainLoop"));
	}

	/**
		interface Iiiinterface { }
		class Main implements Iii{-1-} {
			static function main() {}
		}
	**/
	function testImplements() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "type", "Iiiinterface"));
	}
}
