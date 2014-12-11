package unit.issues;

class TT{}

@:generic
private class C1<T> {
	public var ot:Int;
	public function new() {
		ot = 12;
	}
}

//@:generic
private class SC1<T> extends C1<T>{
	public function new(){
		super();
	}
}

@:generic
private class C2<T>{
	var t:T;
	public function new(t) {
		this.t = t;
	}
}

@:generic
private class SC2<T> extends C2<SC1<T>>{
	public function f(){
		return t.ot;
	}
	public function new(){
		super(new SC1());
	}
}

class Issue3109 extends Test {
	function test() {
		var v:SC2<TT> = new SC2<TT>();
		eq(12, v.f());
	}
}