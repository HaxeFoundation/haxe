package unit.issues;

class Issue3987 extends Test {
#if jvm
	public function test() {
		var int =  java.lang.Integer.valueOf(123);
		eq(int.intValue(), 123);
		t(java.lang.Integer.MIN_VALUE < 0);
		t(java.lang.Integer.MAX_VALUE > 0);
		eq(java.lang.Integer.SIZE, 32);
	}
#end
}
