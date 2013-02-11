package unit;

class TestPhp extends Test
{
	function testAbstractEnum()
	{
		eq(Abstract.getName(), "Abstract");
	}
}

enum Annotation {
	Abstract;
}