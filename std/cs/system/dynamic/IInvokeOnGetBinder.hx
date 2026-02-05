package cs.system.dynamic;

/** Represents information about a dynamic get member operation that indicates if the get member should invoke properties when they perform the get operation. */
@:native("System.Dynamic.IInvokeOnGetBinder")
extern interface IInvokeOnGetBinder {
	/**
	 * Gets the value indicating if this get member operation should invoke properties
	 * when they perform the get operation. The default value when this interface is
	 * not present is true.
	 * @return if this get member operation should invoke properties when they perform
	 * the get operation; otherwise, .
	 */
	var InvokeOnGet(default, never):Bool;
}
