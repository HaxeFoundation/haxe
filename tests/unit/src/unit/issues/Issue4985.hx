package unit.issues;

class Issue4985<T:IDestroyable> extends Test {
	var obj:T;

	function test() {
		#if !php
		try {
			obj.destroy();
		} catch (e:Dynamic) {}
		#end
	}
}

interface IDestroyable {
	public function destroy():Void;
}