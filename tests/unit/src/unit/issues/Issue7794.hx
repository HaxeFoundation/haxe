package unit.issues;
import unit.Test;

private class AudioNode {}
private class AudioParam {}

private class AudioContext {
	public function new() {}
	public var destination:AudioNode;

	#if js
	@:overload(function(destination:AudioNode, output:Int = 0, input:Int = 0):Void {})
	public static function connectMeta(destination:AudioParam, output:Int = 0):Void {}
	#end

	public static extern inline overload function connect(destination:AudioNode, output:Int = 0, input:Int = 0):Void {}
	public static extern inline overload function connect(destination:AudioParam, output:Int = 0):Void {}
}

class Issue7794 extends Test {
	function test() {
		var ctx = new AudioContext();
		AudioContext.connect(ctx.destination);
		noAssert();
	}

	#if js
	function testMeta() {
		var ctx = new AudioContext();
		AudioContext.connectMeta(ctx.destination);
		noAssert();
	}
	#end
}
