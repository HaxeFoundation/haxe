package unit.issues;

class Issue5108 extends Test {
	function test() {
		// nothing to do here
		noAssert();
	}
}

class Signal implements ISignal {
	public function add(listener:()->Void):Void {}
	public function destroy():Void {}
}

interface ISignal extends IDestroyable2 {
	public function add(listener:()->Void):Void;
}

interface IDestroyable2 {
	public function destroy():Void;
}