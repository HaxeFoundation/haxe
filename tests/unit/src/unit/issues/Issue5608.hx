package unit.issues;

class Issue5608 extends unit.Test {
	var fields:Array<String>;

	function test() {
		//should not throw
		fields = Reflect.fields(Issue5608);
		t(true);
	}

	public function resolve(_) return true;
}