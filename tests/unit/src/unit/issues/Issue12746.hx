package unit.issues;

private class Event {}
private class MouseEvent extends Event {}
private class Foo {
	public function onMouseMove(e:MouseEvent):Void {}

	public function new() {}
}

class Issue12746 extends Test {
	function test() {
#if !neko
		final obj = new Foo();

		final a:Event->Void = cast obj.onMouseMove;
		final b:Event->Void = cast obj.onMouseMove;
		t(a == b);

		t(genericCast(obj.onMouseMove, obj.onMouseMove));
#else
		utest.Assert.pass(); // See PR 12763 discussion
#end
	}

	static function genericCast<T:Event>(a:T->Void, b:T->Void):Bool {
		var ca:Event->Void = cast a;
		var cb:Event->Void = cast b;
		return ca == cb;
	}
}
