package unit.issues;

private abstract A(Int) from Int {
	inline public function new(x:Int) this=x;
	@:from public static function fromArr(x:Array<Int>) return new A(x[0]);
	public function toString() {
		return 'A($this)';
	}
}

private abstract M<T>(Null<T>) from Null<T> {
	inline public function new(x:T) this=x;
	public function toString() {
		return 'M($this)';
	}
}

private abstract A2(Int) from Int {
	inline public function new(x:Int) this=x;
	@:to public function fromArr():Array<Int> return [this];
	public function toString() {
		return 'A2($this)';
	}
}

private abstract M2<T>(T) to T {
	inline public function new(x:T) this=x;
	public function toString() {
		return 'M2($this)';
	}
}

class Issue1845 extends Test {
	function test() {
		var m:M<A> = [10];
		eq("M(10)", m.toString());

		var m2:M2<A2> = new M2(12);
		var a:Array<Int> = m2;
		eq(12, a[0]);
	}
}