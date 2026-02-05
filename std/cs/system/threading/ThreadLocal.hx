package cs.system.threading;

@:native("System.Threading.ThreadLocal")
extern class ThreadLocal<T> {
	var IsValueCreated(default, never):Bool;
	var Value(default, default):T;
	var Values(default, never):cs.system.collections.generic.IList<T>;
	@:overload(function():Void {})
	@:overload(function(trackAllValues:Bool):Void {})
	@:overload(function(valueFactory:cs.system.Func_1<T>):Void {})
	function new(valueFactory:cs.system.Func_1<T>, trackAllValues:Bool):Void;
	function Dispose():Void;
	function ToString():String;
}
