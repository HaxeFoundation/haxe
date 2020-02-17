package unit.issues;

import haxe.Json;

class Issue8979 extends unit.Test {
#if cs
	function test() {
		var d = new DummyStruct();
		d.hello = 'world';
		var json = Json.stringify(d);
		eq('{"hello":"world"}', json);
		eq('world', Json.parse(json).hello);
	}
#end
}

#if cs
@:struct
private class DummyStruct {
	public var hello:String;
	public function new() {}
}
#end

