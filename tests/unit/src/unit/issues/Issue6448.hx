package unit.issues;

#if js
@:native("some6448")
private class C {
	public var v = true;
	public function new() {}
}

@:native("")
private extern class Lib {
	@:native("___hx_returnTrue")
	static function returnTrue():Bool;

	static function __init__():Void {
		js.Syntax.code("function ___hx_returnTrue() { return true; }");
	}
}

#elseif python

@:native("")
private extern class Ext {
	static public function len(s:String):Int;
}

#elseif php

@:native("")
private extern class Ext {
	static public function strlen(s:String):Int;
}

#end

class Issue6448 extends unit.Test {
	#if js
	@:analyzer(no_local_dce)
	function test() {
		var some6448 = null;
		t(new C().v);

		function ___hx_returnTrue() return false;
		t(Lib.returnTrue());
	}

	#elseif python

	function test() {
		eq(4, Ext.len('1234'));
	}

	#elseif php

	function test() {
		eq(4, Ext.strlen('1234'));
	}
	#end
}
