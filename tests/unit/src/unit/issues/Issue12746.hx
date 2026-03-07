package unit.issues;

private class Event {}
private class MouseEvent extends Event {}
private class Foo {
	public function onMouseMove(e:MouseEvent):Void {}

	public function new() {}
}

class Issue12746 extends Test {
	function test() {
		final obj = new Foo();

		t(genericCast(obj.onMouseMove, obj.onMouseMove));
	}

	static function genericCast<T:Event>(a:T->Void, b:T->Void):Bool {
		var ca:Event->Void = cast a;
		var cb:Event->Void = cast b;
		return ca == cb;
	}
}
