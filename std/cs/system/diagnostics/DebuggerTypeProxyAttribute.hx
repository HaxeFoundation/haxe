package cs.system.diagnostics;

/** Specifies the display proxy for a type. */
@:native("System.Diagnostics.DebuggerTypeProxyAttribute")
extern class DebuggerTypeProxyAttribute extends cs.system.Attribute {
	/**
	 * Gets the type name of the proxy type.
	 * @return The type name of the proxy type.
	 */
	var ProxyTypeName(default, never):String;
	/**
	 * Gets or sets the target type for the attribute.
	 * @return The target type for the attribute.
	 */
	var Target(default, default):cs.system.Type;
	/**
	 * Gets or sets the name of the target type.
	 * @return The name of the target type.
	 */
	var TargetTypeName(default, default):String;
	@:overload(function(typeName:String):Void {})
	function new(type:cs.system.Type):Void;
}
