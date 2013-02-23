package unit;

class TestPhp extends Test
{
	function empty() return true;
	function testAbstractEnum()
	{
		eq(Abstract.getName(), "Abstract");
	}

	function testAbstractKeywordAsFunction()
	{
		t(empty());
	}
}

enum Annotation {
	Abstract;
}