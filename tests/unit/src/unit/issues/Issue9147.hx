package unit.issues;

import unit.Test;

class Issue9147 extends Test {
	public function test() {
		var result = unit.issues.misc.Issue9147Macro.typeAndReplaceTypes((null:{a:Bool,b:Int}), 'String');
		aeq(['Bool', 'Int'], result.foundTypes);
		eq('{a:String,b:String}', result.mappedAnon);
	}

}