package unit.issues;

class Issue2587 extends Test {
	var e:E;
	function test() {
		e = A;
		if (e == B) return;
		if (e == B) return;
		var arr = [1, 2, 3, 4, 5];
		arr = arr.map(add.bind(_, "test"));
		eq(5, arr.length);
		eq(2, arr[0]);
		eq(3, arr[1]);
		eq(4, arr[2]);
		eq(5, arr[3]);
		eq(6, arr[4]);
	}

	static function add(i:Int, s:String) return i + 1;
}

private enum E {
    A;
    B;
}