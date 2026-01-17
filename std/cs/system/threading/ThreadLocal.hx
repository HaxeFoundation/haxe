package cs.system.threading;

@:native("System.Threading.ThreadLocal")
extern class ThreadLocal<T> {
	var Value:T;

	function new():Void;
	function Dispose():Void;
}
