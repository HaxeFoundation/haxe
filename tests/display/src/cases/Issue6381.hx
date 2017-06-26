package cases;

class Issue6381 extends DisplayTestCase {
	/**
	import haxe.ds.Option;

	class Main {
		public static function main() {
			switch (Some(Some("foo"))) {
				case Some({-1-}in{-2-}ner{-3-} = Some(_)):
					{-4-}inner{-5-};
				case _:
			}
		}
	}
	**/
	function test() {
		eq("haxe.ds.Option<String>", type(pos(2)));
		eq(range(1, 3), position(pos(2)));
		eq(range(4, 5), usage(pos(2))[0]);
	}
}
