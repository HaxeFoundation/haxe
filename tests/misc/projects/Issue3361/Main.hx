interface I {
	public var v:Dynamic->Void;
}

class C implements I {
	public var v:String->Void;
	public function new() { }
}

class Main {
	static function main() {
		var c = new C();
		var i:I = c;
		i.v = function(i:Main) { trace(i); }
		c.v("foo");
	}
}