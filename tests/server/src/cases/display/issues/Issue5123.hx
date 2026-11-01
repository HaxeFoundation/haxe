package cases.display.issues;

class Issue5123 extends DisplayTestCase {
	/**
		class Main {
		    static function main() {
		        var myShinyVar = 50;

		        function {-2-}doBeautifulThings{-3-}(who:String) {
		            return who.length;
		        }

		        {-1-}doBeautifulThings(myShinyVar);
		    }
		}
	**/
	function testLocalFunctionGotoDefinition(_) {
		Assert.same(range(2, 3), position(1));
	}
}
