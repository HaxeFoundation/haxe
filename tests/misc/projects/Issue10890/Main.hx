extern class Main {
	overload function test(b:Bool):Void;
	@:require(false) overload function test(i:Int):Void;
	@:require(false) overload function test(s:String):Void;
}
