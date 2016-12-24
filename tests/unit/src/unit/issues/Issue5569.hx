package unit.issues;

import haxe.Constraints;

class Issue5569 extends unit.Test {
	function test() {
		var namesToFunctions:Map<String, Function> = new Map<String, Function>();
		var noArgs:String = "noArgs";
		var withArgs:String = "withArgs";
		namesToFunctions.set(noArgs, function() {});
		namesToFunctions.set(withArgs, function(a:Int) {});

		namesToFunctions.get(noArgs)();
		namesToFunctions.get(withArgs)(1);

		t(true);
	}
}