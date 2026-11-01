import haxe.Constraints;

class Main {
	static public function main() {
		create(C);
	}

	@:generic
	public static function create<T:Constructible<(a:Bool)->Void>>(type:Class<T>):Void {
		var string = new T("test");
		var bool = new T(true);
	}
}

class C {
	public function new(a:Bool) {

	}
}
