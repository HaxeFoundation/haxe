package cs.system;

@:native("System.Lazy`1")
extern class Lazy_1<T> {
	var IsValueCreated(default, never):Bool;
	var Value(default, never):T;
	@:overload(function():Void {})
	@:overload(function(isThreadSafe:Bool):Void {})
	@:overload(function(valueFactory:cs.system.Func_1<T>):Void {})
	@:overload(function(mode:cs.system.threading.LazyThreadSafetyMode):Void {})
	@:overload(function(value:T):Void {})
	@:overload(function(valueFactory:cs.system.Func_1<T>, isThreadSafe:Bool):Void {})
	function new(valueFactory:cs.system.Func_1<T>, mode:cs.system.threading.LazyThreadSafetyMode):Void;
	function ToString():String;
}
