package unit.issues;

class Issue5894 extends unit.Test {
	function test() {
		var recursed = false;
		function local(o:Array<Dynamic>) {
			for(e in o) {
				if(!recursed) {
					recursed = true;
					return local(e);
				} else {
					return e;
				}
			}
			return 0;
		}
		eq(1, local([[1], 2]));
	}
}