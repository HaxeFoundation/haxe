package cases;

class Issue5999 extends DisplayTestCase {
	/**
		class Main {
			static inline var {-1-}value{-2-} = 1;

			static public function main() {
				trace(val{-3-}ue);
			}
		}
	**/
	function test() {
		eq(range(1, 2), position(pos(3)));
	}
}
