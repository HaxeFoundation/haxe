package unit.issues;

class Issue3512 extends Test {
	function test()
	{
		arr = getArr();
		eq(arr[0], 1);
		t((arr[0] is Int));
		var arr2:Array<Int> = getArr();
		eq(arr2[0], 1);
		t((arr2[0] is Int));
		arr2 = getArr();
		eq(arr2[0], 1);
		t((arr2[0] is Int));
		var arr3:Array<Float> = getArr();
		eq(arr3[0], 1);
		t((arr3[0] is Float));
		arr3 = getArr();
		eq(arr3[0], 1);
		t((arr3[0] is Float));
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

