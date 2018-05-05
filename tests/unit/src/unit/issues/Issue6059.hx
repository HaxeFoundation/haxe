package unit.issues;

class Issue6059 extends Test {
#if !as3 // See #6891
	public static inline function foo (name : B, ?id : B, data : Array<String>) : Void { }

	public static function test () : Void {
		Issue6059.foo ("", []); // -> stackoverflow
		Issue6059.foo ("", null, []); // ok
	}
#end
}

private abstract A (String) {
	public inline function new (value : String) {
		this = value;
	}
	@:from
		public static function fromB (value : B) : A {
			return new A (Std.string (value));
		}
}

private abstract B (String) {
	public inline function new (value : String) {
		this = value;
	}
	@:from
		public static function fromString (value : String) : B {
			return new B (value);
		}
	@:from
		public static function fromA (value : A) : B  {
			return new B (Std.string (value));
		}
}