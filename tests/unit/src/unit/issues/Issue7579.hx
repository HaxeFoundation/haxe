package unit.issues;

private class A<T>
{
    private var value:T;

    public function new()
    {
    }

    public function toB():B<T>
    {
        var b:B<T> = new B<T>();
        b.set(value);
        return b;
    }
}

private class B<T>
{
    private var hash:Map<Int, T>;

    public function new()
    {
        this.hash = new Map<Int, T>();
    }

    public inline function set(item:T):Void
    {
        hash.set(0, item);
    }
}

class Issue7579 extends unit.Test {
	function test() {
		var b = new B<Int>();
		noAssert();
	}
}