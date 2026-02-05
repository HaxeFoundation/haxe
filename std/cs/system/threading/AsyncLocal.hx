package cs.system.threading;

@:native("System.Threading.AsyncLocal")
extern class AsyncLocal<T> {
	var Value(default, default):T;
	@:overload(function():Void {})
	function new(valueChangedHandler:cs.system.Action_1<cs.system.threading.AsyncLocalValueChangedArgs<T>>):Void;
}
