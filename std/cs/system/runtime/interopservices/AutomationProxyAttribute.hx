package cs.system.runtime.interopservices;

/** Specifies whether the type should be marshaled using the Automation marshaler or a custom proxy and stub. */
@:native("System.Runtime.InteropServices.AutomationProxyAttribute")
extern class AutomationProxyAttribute extends cs.system.Attribute {
	/**
	 * Gets a value indicating the type of marshaler to use.
	 * @return if the class should be marshaled using the Automation Marshaler;  if a
	 * proxy stub marshaler should be used.
	 */
	var Value(default, never):Bool;
	function new(val:Bool):Void;
}
