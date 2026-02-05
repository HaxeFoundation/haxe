package cs.system;

/**
 * References a method to be called when a corresponding asynchronous operation
 * completes.
 * @param ar The result of the asynchronous operation.
 */
@:native("System.AsyncCallback")
extern class AsyncCallback extends cs.system.MulticastDelegate {
	function new(func:(ar:cs.system.IAsyncResult)->Void):Void;
	function Invoke(ar:cs.system.IAsyncResult):Void;
}
