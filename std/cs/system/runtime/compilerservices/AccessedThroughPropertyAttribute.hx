package cs.system.runtime.compilerservices;

/** Specifies the name of the property that accesses the attributed field. */
@:native("System.Runtime.CompilerServices.AccessedThroughPropertyAttribute")
extern class AccessedThroughPropertyAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of the property used to access the attributed field.
	 * @return The name of the property used to access the attributed field.
	 */
	var PropertyName(default, never):String;
	function new(propertyName:String):Void;
}
