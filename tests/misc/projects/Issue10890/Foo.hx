extern class Main {
	@:require(true) function test(i:Int):Void;
	@:require(true) function test(s:String):Void;
}

class Bar {
	function bar():Void {}
	@:require(true) static function bar():Void {}
	@:require(true) var bar:String;
}
