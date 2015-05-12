package unit.issues;
import haxe.ds.Vector;

class Issue3248 extends Test {
	public function test()
	{
		var a:AbstractBar<Foo> = new Bar<Foo>(new Vector(4));
		eq(4,a.bar());
	}
}

private class Foo {}

#if !cpp @:nativeGen #end private class Bar<T>
{
	public var arr:Vector<T>;
	public function new(arr)
	{
		this.arr = arr;
	}
}

abstract AbstractBar<T>(Bar<T>) from Bar<T>
{
	public function new(ethis)
	{
		this= ethis;
	}

	public function bar()
	{
		return this.arr.length;
	}
}
