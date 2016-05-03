package unit.issues;

class Issue5192 extends Test {
	public function test() {
		var x:String = Issue5192_Test.doSomething();
		eq(x,null);
	}
}
