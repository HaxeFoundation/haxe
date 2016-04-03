package unit.issues;

class Issue4985<T:IDestroyable> extends Test {
	var obj:T;

	function test() {
		try {
			obj.destroy();
		} catch (e:Dynamic) {}
	}
}

interface IDestroyable {
	public function destroy():Void;
}