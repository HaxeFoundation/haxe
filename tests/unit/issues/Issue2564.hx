package unit.issues;
import unit.Test;

private abstract A1<T>(Array<T>) {
	public function new(arr:Array<T>):Void {
		this = arr;
	}

	public var length(get, never):Int;
	inline function get_length() return this.length;

	@:arrayAccess inline function access(key:Int):T return this[key];
}

//@:coreType
//@:arrayAccess
//private abstract A2<T> from Int {
	//public var length(get, never):Int;
	//inline function get_length():Int return cast this;
//}

class Issue2564 extends Test {
	function test() {
		var acc = "";
		for (i in new A1([1, 2, 3])) {
			acc += ";" + i;
		}
		eq(";1;2;3", acc);
		
		// can't really test this cross-target
		//for (i in (0 : A2<Int>)) {
			//
		//}
		
		var vec = new haxe.ds.Vector(3);
		vec[0] = 1;
		vec[1] = 2;
		vec[2] = 3;
		var acc = "";
		for (i in vec) {
			acc += ";" + i;
		}
		eq(";1;2;3", acc);
	}
}