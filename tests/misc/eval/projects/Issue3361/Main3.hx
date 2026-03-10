interface I {
    public var v(default, never):Dynamic;
}

class C implements I {
    public var v:String;
    public function new() { }
}

interface I2 {
	public var v(never, default):String;
}

class C2 implements I2 {
	public var v:Dynamic;
	public function new() { }
}

class Main3 {
    static function main() {
        var c = new C();
        var i:I = c;
		c.v = "foo";
		i.v;

		var c2 = new C2();
		var i2:I2 = c2;
		i2.v = "foo";
		c.v;
    }
}