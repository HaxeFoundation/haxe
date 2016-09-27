package unit.issues;


class Issue2936 extends Test {
	function test() {
		f([1,2,3] == [1,2,3]);
		f(([1,2,3]:Dynamic) == ([1,2,3]:Dynamic));
		f(([1,2,3]:Dynamic) == [1,2,3]);
		f([1,2,3] == ([1,2,3]:Dynamic));

		t([1,2,3] != [1,2,3]);
		t(([1,2,3]:Dynamic) != ([1,2,3]:Dynamic));
		t(([1,2,3]:Dynamic) != [1,2,3]);
		t([1,2,3] != ([1,2,3]:Dynamic));
	}
}