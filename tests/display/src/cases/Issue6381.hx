package cases;

class Issue6381 extends DisplayTestCase {
	/**
	import haxe.ds.Option;

	class Main {
		public static function main() {
			switch (Some(Some("foo"))) {
				case Some(in{-1-}ner = Some(_)):
				case _:
			}
		}
	}
	**/
	function test() {
		eq("haxe.ds.Option<String>", type(pos(1)));
	}
}
