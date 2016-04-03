package unit.issues;

class Issue3987 extends Test {
#if java
	public function test() {
		var int =  java.lang.Integer.valueOf(123);
		eq(int.intValue(), 123);
	}
#end
}
