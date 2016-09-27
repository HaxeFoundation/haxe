package unit.issues;

class Issue4460 extends Test
{
	public function test()
	{
		var arr = new LikeArray([1,2,3,4]);
		arr = arr.slice(1, 2);
		var arr2 = arr.arr();
		eq(arr2[0], 2);
	}
}

private abstract LikeArray<T>(Array<T>)
{
	inline public function new(arr)
	{
		this = arr;
	}

	public function slice(pos:Int, ?end:Int):LikeArray<T>
		return new LikeArray(this.slice(pos,end));

	public function arr()
	{
		return this;
	}
}
