package cs.system.threading;

/** Represents the method that executes on a . */
@:native("System.Threading.ThreadStart")
extern class ThreadStart extends cs.system.MulticastDelegate {
	function new(func:()->Void):Void;
	function Invoke():Void;
}
