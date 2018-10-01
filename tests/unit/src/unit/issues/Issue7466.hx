package unit.issues;
import unit.issues.misc.Issue7466Macro as Macro;

class Issue7466 extends unit.Test {
	function test() {
		f(Macro.canType( Macro.getCheckType() ));
	}
}