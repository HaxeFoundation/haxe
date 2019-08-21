class Main {
	static function main() {
		var a = ["hello"];
		sortArray(a); // works properly
		sortArrayInline(a); // generates wrong code
	}

	static function sortArray<T>(a:Array<T>):Void {
		cs.system.Array.Sort(@:privateAccess a.__a, 0, a.length);
	}

	static inline function sortArrayInline<T>(a:Array<T>):Void {
		cs.system.Array.Sort(@:privateAccess a.__a, 0, a.length);
	}
}
