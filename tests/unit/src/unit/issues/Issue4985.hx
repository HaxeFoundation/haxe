package unit.issues;

class Issue4985<T:IDestroyable> extends Test {
	function test() {
		testDestroy(new Destroyable());
	}

	function testDestroy(d:T) {
		#if !php
		try {
			d.destroy();
		} catch (e:Dynamic) {}
		#end
	}
}

interface IDestroyable {
	public function destroy():Void;
}

class Destroyable implements IDestroyable {
	public function new() {}
	public function destroy():Void {}
}