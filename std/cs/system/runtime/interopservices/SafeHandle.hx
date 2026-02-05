package cs.system.runtime.interopservices;

/** Represents a wrapper class for operating system handles. This class must be inherited. */
@:native("System.Runtime.InteropServices.SafeHandle")
extern class SafeHandle extends cs.system.runtime.constrainedexecution.CriticalFinalizerObject {
	/**
	 * Gets a value indicating whether the handle is closed.
	 * @return if the handle is closed; otherwise, .
	 */
	var IsClosed(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the handle
	 * value is invalid.
	 * @return if the handle value is invalid; otherwise, .
	 */
	var IsInvalid(default, never):Bool;
	/** Marks the handle for releasing and freeing resources. */
	function Close():Void;
	/**
	 * Manually increments the reference counter on  instances.
	 * @param success if the reference counter was successfully incremented; otherwise,
	 * .
	 */
	function DangerousAddRef(success:cs.Ref<Bool>):Void;
	/**
	 * Returns the value of the  field.
	 * @return An  representing the value of the  field. If the handle has been marked
	 * invalid with , this method still returns the original handle value, which can be
	 * a stale value.
	 */
	function DangerousGetHandle():cs.system.IntPtr;
	/** Manually decrements the reference counter on a  instance. */
	function DangerousRelease():Void;
	/** Releases all resources used by the  class. */
	function Dispose():Void;
	/** Marks a handle as no longer used. */
	function SetHandleAsInvalid():Void;
}
