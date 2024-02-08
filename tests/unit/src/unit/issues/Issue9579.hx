package unit.issues;
import unit.Test;

private class Base {
	public function new(o:Base) {}
}

private class A extends Base {
	public function new(o:A) {
		super(o);
	}
}

private class B extends Base {}

class Issue9579 extends Test {
	public function test() {
		var a = [A.new, B.new];
		HelperMacros.typedAs(a, ([] : Array<(o:A) -> Base>));
	}
}