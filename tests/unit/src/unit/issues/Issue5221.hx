package unit.issues;

class Issue5221 extends unit.Test {
	static var ip(default,default):Int = -1;
	function test() {
		var ip2 = ip;
		ip2 = ip = 0;
		eq(0, ip2);
		eq(0, ip);
	}
}