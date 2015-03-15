package unit.issues;

class Issue2282 extends Test
{
    function test() {
			var f = new Foo();
			eq(null,f.val);
    }
}


private class Foo<T>
{
	public var val(get,null):T;
	public function new()
	{
	}

	public function get_val():T
	{
		return null;
	}
}

