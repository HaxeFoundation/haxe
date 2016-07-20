package unit.issues;
class Issue5323 extends Test{
	function new() super();
    function test() {
    	var expected = 1;
        var actual = new Issue5323().doStuff(function() return 0);
        eq(actual, expected);
    }

    function doStuff(self) {
        return doOtherStuff();
    }

    function doOtherStuff() {
    	return 1;
    }
}
