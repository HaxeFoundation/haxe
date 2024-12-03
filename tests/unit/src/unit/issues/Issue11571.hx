package unit.issues;

class Issue11571 extends unit.Test {
	public function test() {
		var fOk = () -> "ok";
		var f:Null<() -> String> = fOk;
		var boundOk = f?.bind();
		f = null;
		eq("ok", boundOk());

		var boundNull = f?.bind();
		f = fOk;
		eq(null, boundNull());
	}
}
