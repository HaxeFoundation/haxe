package unit.issues;
import unit.Test;

private class Base { }

private class Child extends Base {}

@:analyzer(no_check_has_effect)
class Issue2675 extends Test {
	function test() {
		t(unit.TestType.typeError(
			try { }
			catch(e:Base) { }
			catch(e:Child) { }
		));

		t(unit.TestType.typeError(
			try { }
			catch(e:Dynamic) { }
			catch(e:Child) { }
		));

		t(unit.TestType.typeError(
			try { }
			catch(e:Dynamic) { }
			catch(e:Dynamic) { }
		));

		try { }
		catch(e:Child) { }
		catch(e:Base) { }
		catch(e:Dynamic) { }
	}
}