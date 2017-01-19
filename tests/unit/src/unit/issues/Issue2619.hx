package unit.issues;
import unit.Test;

private abstract A(String) to String {
	public function new(s) this = s;
}

private abstract B(String) {
	public function new(s) this = s;
}

class Issue2619 extends Test {
	function test() {
		var s:String = try {
			throw "foo";
		} catch(e:A) {
			e;
		}
		eq("foo", s);

		var s:String = try {
			throw new A("foo");
		} catch(e:String) {
			e;
		}
		eq("foo", s);

		t(unit.HelperMacros.typeError(
			try { }
			catch(e:A) { }
			catch(e:B) { }
		));
	}
}