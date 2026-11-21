// a type reached through a recursive typedef, or directly
typedef F = (F, C) -> Void;

class C {
	public var f:F;

	public function new() {}
}

@:build(Macro.statics(600))
class ReachThroughTypedef {
	public static function a0(f:F):Void {
		if (f != null)
			f(f, null);
	}
}

class ReachDirectly {
	public static function run(arr:Array<Int>) {
		var x = Macro.long(arr, 3000);
		return x > 0 ? new C() : null;
	}
}
