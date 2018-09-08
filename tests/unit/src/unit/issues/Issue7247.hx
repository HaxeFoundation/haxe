package unit.issues;
import unit.Test;

class Issue7247 extends Test {

	var str = "将一";
	
	function test() {
		eq(str.indexOf("\\"),-1);
		eq(str.lastIndexOf("\\"),-1);
		eq(str.split("\\").length,1);
		f( ~/\\/.match(str) );
	}

}