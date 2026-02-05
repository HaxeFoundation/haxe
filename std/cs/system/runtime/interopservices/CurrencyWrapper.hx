package cs.system.runtime.interopservices;

/** Wraps objects the marshaler should marshal as a . */
@:native("System.Runtime.InteropServices.CurrencyWrapper")
extern class CurrencyWrapper {
	/**
	 * Gets the wrapped object to be marshaled as type .
	 * @return The wrapped object to be marshaled as type .
	 */
	var WrappedObject(default, never):cs.system.Decimal;
	@:overload(function(obj:cs.system.Decimal):Void {})
	function new(obj:Dynamic):Void;
}
