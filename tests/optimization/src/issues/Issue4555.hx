package issues;

private abstract A(Null<Float>) from Null<Float> {
	@:to public inline function toString():String {
		if (this == null) {
			throw "Something went wrong";
		} else {
			return Std.string(this);
		}
	}
}

class Issue4555 {
	@:js('
		var s;
		if(a == null) {
			throw haxe_Exception.thrown("Something went wrong");
		} else {
			s = Std.string(a);
		}
		issues_Issue4555.use(s);
	')
	static function test(a:A) {
		var s:String = a;
		use(s);
	}

	@:pure(false)
	static function use(v:String) {}
}
