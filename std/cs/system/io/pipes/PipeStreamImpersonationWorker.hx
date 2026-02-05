package cs.system.io.pipes;

/** Represents the method to call as the client. */
@:native("System.IO.Pipes.PipeStreamImpersonationWorker")
extern class PipeStreamImpersonationWorker extends cs.system.MulticastDelegate {
	function new(func:()->Void):Void;
	function Invoke():Void;
}
