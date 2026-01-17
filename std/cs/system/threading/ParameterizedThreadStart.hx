package cs.system.threading;

@:native("System.Threading.ParameterizedThreadStart")
extern class ParameterizedThreadStart {
	function new(callback:(Dynamic) -> Void):Void;
}
