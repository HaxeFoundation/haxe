package unit.issues;

class Issue3132 extends unit.Test {
	function test() {
		var a:Array<{a:Int, ?b:Int}> = [{a:1}, {a:1, b:2}];
		var a:Array<{a:Int, ?b:Int}> = [{a:1, b:2}, {a:1}];
		//t(unit.HelperMacros.typeError([{a:1}, {a:1, b:2}]));
		//t(unit.HelperMacros.typeError([{a:1, b:2}, {a:1}]));
	}
}