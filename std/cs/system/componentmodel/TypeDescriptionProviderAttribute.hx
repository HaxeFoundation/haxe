package cs.system.componentmodel;

/** Specifies the custom type description provider for a class. This class cannot be inherited. */
@:native("System.ComponentModel.TypeDescriptionProviderAttribute")
extern class TypeDescriptionProviderAttribute extends cs.system.Attribute {
	/**
	 * Gets the type name for the type description provider.
	 * @return A  containing the qualified type name for the .
	 */
	var TypeName(default, never):String;
	@:overload(function(typeName:String):Void {})
	function new(type:cs.system.Type):Void;
}
