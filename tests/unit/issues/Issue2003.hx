package unit.issues;
import unit.Test;

class Issue2003 extends Test {

	var myVar:String = "cool";

	function test() {
		var o = unit.issues.misc.Issue2003Macro.create().callMe( function() { this.myVar = "super "+myVar; } ).testMacro();
		eq("super cool", myVar);
	}
}