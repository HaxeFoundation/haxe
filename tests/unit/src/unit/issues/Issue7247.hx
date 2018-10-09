package unit.issues;
import unit.Test;

class Issue7247 extends Test {

	#if !neko

	var str = "将一";
	var str2 = "将一\\";

	function test() {
		eq(str.indexOf("\\"),-1);
		eq(str.lastIndexOf("\\"),-1);
		eq(str.split("\\").length,1);
		f( ~/\\/.match(str) );

		eq(str2.indexOf("\\"), 2);
		eq(str2.lastIndexOf("\\"), 2);
		eq(str2.split("\\").length, 2);
		t( ~/\\/.match(str2) );
	}

	#end
}
