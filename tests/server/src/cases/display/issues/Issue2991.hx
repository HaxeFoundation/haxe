package cases.display.issues;

class Issue2991 extends DisplayTestCase {
	/**
		class Main
		{
		    static function f(a:String) {}

		    static function main() {
		        var a = {{-2-}i{-3-}: ""};
		        f(a.{-1-}i);
		    }
		}
	**/
	function testObjectFieldGotoDefinition(_) {
		Assert.same(range(2, 3), position(1));
	}
}
