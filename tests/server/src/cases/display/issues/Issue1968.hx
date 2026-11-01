package cases.display.issues;

class Issue1968 extends DisplayTestCase {
	/**
		using Main.A;

		class A {
		    public static function {-2-}f{-3-}(v:String, b:Int):String return v;
		}

		class Main {
		    static function main() {
		        "a".{-1-}f
		    }
		}
	**/
	function testUsingExtensionGotoDefinition(_) {
		Assert.same(range(2, 3), position(1));
	}
}
