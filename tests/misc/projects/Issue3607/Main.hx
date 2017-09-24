using Main.Tools;

class Tools {
	@:pure(false)
    public static function f(b:Base) {}

	public static var test:String;
}

class Base {
	public function new() { }
}

class Child extends Base {
    public function a() {
        super.f();
    }
}

class Main {
	static public function main() {
		new Child().a();
	}
}