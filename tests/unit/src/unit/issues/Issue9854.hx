package unit.issues;

class Issue9854 extends Test {
#if (cs || java)

	overload
	static function infer<T>(s:String):T {
		return null;
	}

	static inline function inlineMe():String {
		return infer("foo");
	}

	function test() {
		var x = inlineMe();
		eq('String', HelperMacros.typeString(x));
	}
#end
}
