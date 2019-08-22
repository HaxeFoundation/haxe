package cases;

class Issue7906 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var method:Ha{-1-}xeRequestMethod;
				n{-2-}ew Ha{-3-}xeRequestMethod();
			}
		}

		typedef {-4-}HaxeRequestMethod{-5-} = RequestMethod;

		abstract RequestMethod(String) to String {
			public function {-6-}new{-7-}()
				this = "";
		}
	**/
	function test() {
		eq(range(4, 5), position(pos(1)));
		eq(range(6, 7), position(pos(2)));
		eq(range(4, 5), position(pos(3)));
	}
}
