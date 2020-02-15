package cases;

class Issue9142 extends DisplayTestCase {
	/**
		import NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentImport() {
		eq("String", type(pos(1)));
	}
}
