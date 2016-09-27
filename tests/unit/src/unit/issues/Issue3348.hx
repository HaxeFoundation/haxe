package unit.issues;

class Issue3348 extends Test {

	function test() {
		eq("unit.issues.Issue3348,Issue3348.hx,6,test", getPos1());
		eq("unit.issues.Issue3348,Issue3348.hx,7,test", getPos2());
		eq("unit.issues.Issue3348,Issue3348.hx,8,test", getPos3("ok"));
		eq("unit.issues.Issue3348,Issue3348.hx,9,test", getPos4(1));
	}

	static function getPosString(p:haxe.PosInfos) {
		return [p.className, p.fileName, Std.string(p.lineNumber), p.methodName].join(",");
	}

	static function getPos1(?p:haxe.PosInfos) {
		return getPosString(p);
	}

	static function getPos2(?s:String, ?p:haxe.PosInfos) {
		return getPosString(p);
	}

	static function getPos3(s:String, ?p:haxe.PosInfos) {
		return getPosString(p);
	}

	static function getPos4(?s:String, ?i:Int, ?p:haxe.PosInfos) {
		return getPosString(p);
	}
}