package unit.issues;

class Issue5108 extends Test {
    function test() {
		// nothing to do here
	}
}

class Signal implements ISignal {   
    public function add(listener:Void->Void):Void {}
    public function destroy():Void {}
}

interface ISignal extends IDestroyable {
    public function add(listener:Void->Void):Void;
}

interface IDestroyable {
    public function destroy():Void;
}