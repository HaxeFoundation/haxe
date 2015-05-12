package unit.issues;

class Issue4045 extends Test
{
#if cs
	public function test()
	{
		var t = new TestS();
		t.a = 10;
		eq(10, t.a);
	}
#end
}

#if cs
private abstract TestS(TestClass)
{
    inline public function new()
    {
        this = new TestClass(0);
    }

    public var a(get,set):Int;

    inline private function get_a()
    {
        return this.a;
    }

    inline private function set_a(v:Int)
    {
        return this.a = v;
    }
}

@:nativeGen @:struct private class TestClass
{
    public var a:Int;

    public function new(av)
    {
        this.a = av;
    }
}
#end
