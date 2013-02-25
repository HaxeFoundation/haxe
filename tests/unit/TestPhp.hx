package unit;

class TestPhp extends Test
{
	var list : Array<Dynamic>;
	var list2 : Array<Dynamic>;
	function empty() return true;
	function testAbstractEnum()
	{
		eq(Abstract.getName(), "Abstract");
	}

	function testAbstractKeywordAsFunction()
	{
		t(empty());
	}

	function testList2()
	{
		list2 = new Array<Dynamic>();
		for( l in list2 ) {} // fails here
		t(true);
	}

	function testList()
	{
		list = new Array<Dynamic>();
		for( l in list ) {} // fails here
		t(true);
	}
}

enum Annotation {
	Abstract;
}