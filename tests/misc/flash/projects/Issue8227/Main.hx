private class Child extends NsCls {
	public function new() {
		super();
		var _ns1v:Int = ns1v;
		ns1v = 50;
		var _ns2v:Int = ns2v;
		ns2v = 10;
		var _ns1f:Int = ns1f();
		var _ns1f2:Int = call(ns1f);
		var _ns2f:Int = ns2f();
		var _ns2f2:Int = call(ns2f);
		var _ns1sv:Int = NsCls.ns1sv;
		NsCls.ns1sv = 50;
		var _ns2sv:Int = NsCls.ns2sv;
		NsCls.ns2sv = 10;
		var _ns1sf:Int = NsCls.ns1sf();
		var _ns1sf2:Int = call(NsCls.ns1sf);
		var _ns2sf:Int = NsCls.ns2sf();
		var _ns2sf2:Int = call(NsCls.ns2sf);
	}

	static function call(f:()->Int):Int {
		return f();
	}
}

class Main {
	static function main() {
		var ns = new NsCls();
		var _ns1v:Int = ns.ns1v;
		ns.ns1v = 50;
		var _ns2v:Int = ns.ns2v;
		ns.ns2v = 10;
		new Child();
	}
}
