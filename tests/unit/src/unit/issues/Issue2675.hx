package unit.issues;
import unit.Test;

private class Base { }

private class Child extends Base {}

class Issue2675 extends Test {
	function test() {
		t(unit.HelperMacros.typeError(
			try { }
			catch(e:Base) { }
			catch(e:Child) { }
		));

		t(unit.HelperMacros.typeError(
			try { }
			catch(e:Dynamic) { }
			catch(e:Child) { }
		));

		t(unit.HelperMacros.typeError(
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