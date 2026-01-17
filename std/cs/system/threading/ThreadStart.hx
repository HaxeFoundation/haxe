package cs.system.threading;

@:native("System.Threading.ThreadStart")
extern class ThreadStart {
	function new(callback:() -> Void):Void;
}
