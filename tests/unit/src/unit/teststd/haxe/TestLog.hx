package unit.teststd.haxe;

class TestLog extends unit.Test {
	public function test() {
		var s = "";
		var p:haxe.PosInfos = null;
		var old = haxe.Log.trace;
		haxe.Log.trace = function(v, ?i) {
			s = v;
			p = i;
		}
		trace("test trace");
		haxe.Log.trace = old;
		eq(s, "test trace");
		eq(p.fileName, "src/unit/teststd/haxe/TestLog.hx");
		eq(p.lineNumber, 12);
		haxe.Log.trace = null;
		exc(function() trace("exc test"));
		haxe.Log.trace = old;

	}
}
