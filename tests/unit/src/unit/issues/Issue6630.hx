package unit.issues;

class Issue6630 extends unit.Test {
	#if !cpp
	static var cls:Dynamic;
	static inline var HELLO = 'hello';

	function test() {
		cls = Issue6630;
		eq(cls.HELLO, HELLO);
		eq(Reflect.field(Issue6630, 'HELLO'), HELLO);
		t(Reflect.hasField(Issue6630, 'HELLO'));
	}
	#end
}
