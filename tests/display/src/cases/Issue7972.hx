package cases;

class Issue7972 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var foo = 0.0;

				switch (S{-1-}td.i{-2-}nt(fo{-3-}o)) {
					case _:
				}
			}
		}
	**/
	function test() {
		eq("Class<Std>", type(pos(1)));
		eq("(x : Float) -> Int", type(pos(2)));
		eq("Float", type(pos(3)));
	}
}
