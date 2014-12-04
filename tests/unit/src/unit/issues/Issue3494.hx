package unit.issues;

private abstract E2<T1,T2>(Dynamic)
	from T1 from T2 from E2<T2,T1>
	to T1 to T2 to E2<T2,T1>
{}

class Issue3494 extends Test {
	function test() {
        var v1:E2<Int,String> = 1;
        var v2:E2<String,Int> = 2;
        v1 = v2;
	}
}