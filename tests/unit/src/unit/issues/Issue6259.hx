package unit.issues;
class Issue6259 extends Test{
	function test(){
#if !python
		function f(a) return a.b;
		var res = f({b: (s) -> s.length})("6259");
		eq(res,4);
#end
	}
}
