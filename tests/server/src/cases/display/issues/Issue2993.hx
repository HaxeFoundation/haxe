package cases.display.issues;

class Issue2993 extends DisplayTestCase {
	/**
		class Main {
		    function f({-2-}arg{-3-}) {
		        if ({-1-}arg) trace(arg);
		        arg = false;
		    }
		}
	**/
	function testFunctionParamGotoDefinition(_) {
		Assert.same(range(2, 3), position(1));
	}
}
