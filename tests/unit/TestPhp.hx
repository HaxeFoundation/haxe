package unit;

using StringTools;

class TestPhp extends Test
{
	var list : Array<Dynamic>;
	var list2 : Array<Dynamic>;
	function empty() return true;
	function testAbstractEnum()
	{
		eq(Abstract.getName(), "Abstract");
		var x = Const("foo");
		var s = switch(x) {
			case Const(s): s;
			case Abstract: "null";
		}
		eq("foo", s);
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

	function testIssue1828()
	{
        var x = try {
            throw "foo";
            false;
        } catch (e:String) {
            true;
        }
        t(x);
	}

	function testIssue1521()
	{
		var pattern = "$a$b";
		var result = pattern.replace("$a","A").replace("$b","B");
		eq("AB", result);
	}
}

enum Annotation {
	Abstract;
	Const(i:String);
}