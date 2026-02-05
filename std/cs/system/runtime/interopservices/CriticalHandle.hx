package cs.system.runtime.interopservices;

/** Represents a wrapper class for handle resources. */
@:native("System.Runtime.InteropServices.CriticalHandle")
extern class CriticalHandle extends cs.system.runtime.constrainedexecution.CriticalFinalizerObject {
	/**
	 * Gets a value indicating whether the handle is closed.
	 * @return if the handle is closed; otherwise, .
	 */
	var IsClosed(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the handle
	 * value is invalid.
	 * @return if the handle is valid; otherwise, .
	 */
	var IsInvalid(default, never):Bool;
	/** Marks the handle for releasing and freeing resources. */
	function Close():Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
	/** Marks a handle as invalid. */
	function SetHandleAsInvalid():Void;
}
