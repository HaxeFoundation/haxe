package unit.hxcpp_issues;

class ConstuctorWithArgCalledResult {
   public function new(result:Int) { }
}

class Issue192 extends Test {
	function test() new ConstuctorWithArgCalledResult(1);
}

