package unit.issues;

import String;
import haxe.Template as T;
import haxe.macro.*;

class Issue3560 extends Test {
	function test() {
		var s = unit.issues.misc.Issue3560Macro.getImportString();
		// the order isn't really defined, but whatever
		eq("IAll:haxe.macro", s[0]);
		eq("IAsName(T):haxe.Template", s[1]);
		eq("INormal:String", s[2]);
	}
}