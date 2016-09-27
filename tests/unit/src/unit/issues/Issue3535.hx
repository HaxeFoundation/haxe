package unit.issues;

@:generic
private class IndexedItemIterator<T>
{
	var items:Array<T>;
	var i:Int = 0;

	public inline function new(items:Array<T>)
	{
		this.items = items;
	}

	public inline function hasNext()
	{
		return i < items.length;
	}

	public inline function next()
	{
		return new IndexedItemObject<T>(i, items[i++]);
	}
}

@:generic
private class IndexedItemObject<T>
{
	public var index:Int;
	public var item:T;

	public inline function new(index:Int, item:T)
	{
		this.index = index;
		this.item = item;
	}
}

class Issue3535 extends Test {
	function test() {
		var list = ["111","222","333"];
		var buf = new StringBuf();
		for(value in new IndexedItemIterator<String>(list)) {
			buf.add(value.index + ":" + value.item);
		}

		eq("0:1111:2222:333", buf.toString());

		var list2 = [3,4,5,7];
		var buf = new StringBuf();
		for(value in new IndexedItemIterator<Int>(list2)) {
			buf.add(value.index + ":" + value.item);
		}
		eq("0:31:42:53:7", buf.toString());
	}
}