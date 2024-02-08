package cases;

class Issue5796 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}
		}

		typedef Test = {
			@:gen{-1-}eric var f{-2-}oo:Int;
		}
	**/
	function test() {
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(pos(1)));
		eq("Int", type(pos(2)));
	}
}
