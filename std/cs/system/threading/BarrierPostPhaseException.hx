package cs.system.threading;

/** The exception that is thrown when the post-phase action of a  fails */
@:native("System.Threading.BarrierPostPhaseException")
extern class BarrierPostPhaseException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(innerException:cs.system.Exception):Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
