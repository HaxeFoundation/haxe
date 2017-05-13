package unit.issues;

private abstract S(String) to String {
	@:from static inline function fromString(s:String):S return cast s.toUpperCase();
}

class Issue5507 extends unit.Test {
	#if (cs || java)
	@:readOnly static var a(default,never):S = "hello";

	function test() {
		eq("HELLO", a);
	}
	#end
}
