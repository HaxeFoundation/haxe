package unit.issues;

#if js
@:native("Issue4419External")
private extern class External {
	static function __init__():Void {
		haxe.macro.Compiler.includeFile("unit/issues/misc/Issue4419External.js");
	}
}

private class SubExt extends External {
	public function new():Void {}
	public function hello() return "hello";
}
#end

class Issue4419 extends Test {
	#if js
	function test() {
		eq("hello", new SubExt().hello());
	}
	#end
}