class Main {
	overload
	static function infer<T>(s:String):T {
		return null;
	}

	static inline function inlineMe():String {
		return infer("foo");
	}

	static function main() {
		var x = inlineMe();
		$type(x); // Unknown<0>
	}
}