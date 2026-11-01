interface I {
	public dynamic function f(d:Dynamic):Void;
}

class C implements I {
	public dynamic function f(s:String):Void { }
	public function new() { }
}

class Main2 {
	static function main() {
		var c = new C();
		var i:I = c;
		i.f = function(i:Main2) { trace(i); }
		c.f("foo");
	}
}