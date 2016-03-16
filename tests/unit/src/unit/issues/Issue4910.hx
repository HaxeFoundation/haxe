package unit.issues;

private class C {
	public static function f(i:Int){ return "f"; }
}

private typedef TD = {
	function f(v:Int):String;
}

@:analyzer(no_fusion)
class Issue4910 extends Test {
	function test() {
		var t:TD = C;
		eq("f", t.f(1));
		eq("f", tf(C));
		eq("f", tfd(C));
	}

	static function tf(t:TD){
		return t.f(1);
	}

	static function tfd(t:Dynamic){
		return t.f(1);
	}
}