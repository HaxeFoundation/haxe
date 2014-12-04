package unit.issues;

class Issue3512 extends Test {
	function test()
	{
		arr = getArr();
		eq(arr[0], 1);
		t(Std.is(arr[0],Int));
		var arr2:Array<Int> = getArr();
		eq(arr2[0], 1);
		t(Std.is(arr2[0], Int));
		arr2 = getArr();
		eq(arr2[0], 1);
		t(Std.is(arr2[0], Int));
		var arr3:Array<Float> = getArr();
		eq(arr3[0], 1);
		t(Std.is(arr3[0], Float));
		arr3 = getArr();
		eq(arr3[0], 1);
		t(Std.is(arr3[0], Float));
	}

	static var arr:Array<Int>;

	public static function getArr():Dynamic
	{
		var arr:Array<Dynamic> = [];
		arr.push(1);
		arr.push(2);
		arr.push(3);
		return arr;
	}
}

