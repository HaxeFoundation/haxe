package cs.system.runtime.interopservices;

/** Provides methods that enable .NET Framework delegates that handle events to be added and removed from COM objects. */
@:native("System.Runtime.InteropServices.ComEventsHelper")
extern class ComEventsHelper {
	/**
	 * Adds a delegate to the invocation list of events originating from a COM object.
	 * @param rcw The COM object that triggers the events the caller would like to
	 * respond to.
	 * @param iid The identifier of the source interface used by the COM object to
	 * trigger events.
	 * @param dispid The dispatch identifier of the method on the source interface.
	 * @param d The delegate to invoke when the COM event is fired.
	 */
	static function Combine(rcw:Dynamic, iid:cs.system.Guid, dispid:Int, d:cs.system.Delegate):Void;
	/**
	 * Removes a delegate from the invocation list of events originating from a COM
	 * object.
	 * @param rcw The COM object the delegate is attached to.
	 * @param iid The identifier of the source interface used by the COM object to
	 * trigger events.
	 * @param dispid The dispatch identifier of the method on the source interface.
	 * @param d The delegate to remove from the invocation list.
	 * @return The delegate that was removed from the invocation list.
	 */
	static function Remove(rcw:Dynamic, iid:cs.system.Guid, dispid:Int, d:cs.system.Delegate):cs.system.Delegate;
}
