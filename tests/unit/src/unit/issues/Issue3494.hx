package unit.issues;

private abstract E2<T1,T2>(Dynamic)
	from T1 from T2 from E2<T2,T1>
	to T1 to T2 to E2<T2,T1>
{}

private abstract E3<T1,T2>(Dynamic)
from T1 from T2
to T1 to T2
{
	@:from inline static public function _from2<T1, T2, _T1:E3<T1,T2>, _T2:E3<T1,T2>>(e:E3<_T1,_T2>):E3<T2,T1> {
		return untyped e;
	}
}

class Issue3494 extends Test {
	function test() {
		var v1:E2<Int,String> = 1;
		var v2:E2<String,Int> = 2;
		v1 = v2;
		eq(2, v1);
		eq(2, v2);
	}

	function test2() {
		var v1:E3<Int,String> = 1;
		var v2:E3<String,Int> = 2;
		v1 = v2;
		eq(2, v1);
		eq(2, v2);
	}

	function test3() {
		var v1:E3<Int,String> = 1;
		var v2:E3<String,Int> = 2;
		v1 = E3._from2(v2);
		eq(2, v1);
		eq(2, v2);
	}
}