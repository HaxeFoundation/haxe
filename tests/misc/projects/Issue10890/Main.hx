extern class Main {
	@:require(false) function test(i:Int):Void;
	@:require(true) function test(s:String):Void;
	@:require(true) overload function foo(i:Int):Void;
	@:require(true) overload function foo(s:String):Void;
}

class Bar {
	function bar():Void {}
	@:require(false) static function bar():Void {}
	@:require(false) var bar:String;
}
