package unit.issues;

private enum Foo {
	Bar(i:Int);
}

class Issue9200 extends unit.Test {
	public function test () {
		switch Bar(4) {
			case null:
			case Bar(s): eq(4, s);
		}
	}
}