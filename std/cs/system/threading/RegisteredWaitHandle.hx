package cs.system.threading;

/** Represents a handle that has been registered when calling . This class cannot be inherited. */
@:native("System.Threading.RegisteredWaitHandle")
extern class RegisteredWaitHandle extends cs.system.MarshalByRefObject {
	/**
	 * Cancels a registered wait operation issued by the  method.
	 * @param waitObject The  to be signaled.
	 * @return if the function succeeds; otherwise, .
	 */
	function Unregister(waitObject:cs.system.threading.WaitHandle):Bool;
}
