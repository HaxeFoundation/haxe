package unit.issues;

class Issue5470 extends unit.Test {
  var cb:()->Void;
  
	function test() {
		eq(new Issue5470().cb, null);
	}

}
