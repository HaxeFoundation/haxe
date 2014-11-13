package unit.issues;
import unit.Test;

@:arrayAccess
private abstract MyVector<T>(Array<T>) {

	public function new ():Void {
		this = new Array<T>();
	}

	public function push(x:T):Int {
		return this.push(x);
	}

	@:from static public inline function fromArray<T, U> (a:Array<U>):MyVector<T> {
		return cast a;
	}

	@:to public function toArray<T>():Array<T> {
		return this;
	}
}


class Issue2713 extends Test {
	function test() {
		var v:MyVector<Int> = [1, 2, 3];
		for (i in 0...2) {
			v.push(i);
		}
		eq(1, v[0]);
		eq(2, v[1]);
		eq(3, v[2]);
		eq(0, v[3]);
		eq(1, v[4]);
	}
}