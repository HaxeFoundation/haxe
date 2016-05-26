package unit.issues;
import unit.issues.Issue5192_Test;

class Issue5192 extends Test {
	public function test() {
		var x:String = Issue5192_Test_Abstract.doSomething();
		eq(x,null);
	}
}
