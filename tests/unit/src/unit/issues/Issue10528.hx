package unit.issues;

@:using(unit.issues.Issue10528.ExtensionA)
@:generic
private class Data<T> {
	public var data:T;

	public function new(data)
		this.data = data;
}

private class ExtensionA {
	public static function defaultUsing<D>(d:D) {
		return null;
	}
}

class Issue10528 extends unit.Test {
	private function test() {
		var p = new Data("foo");
		p.defaultUsing();
		utest.Assert.pass();
	}
}
