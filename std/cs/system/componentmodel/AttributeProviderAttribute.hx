package cs.system.componentmodel;

/** Enables attribute redirection. This class cannot be inherited. */
@:native("System.ComponentModel.AttributeProviderAttribute")
extern class AttributeProviderAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of the property for which attributes will be retrieved.
	 * @return The name of the property for which attributes will be retrieved.
	 */
	var PropertyName(default, never):String;
	/**
	 * Gets the assembly qualified type name passed into the constructor.
	 * @return The assembly qualified name of the type specified in the constructor.
	 */
	var TypeName(default, never):String;
	@:overload(function(typeName:String):Void {})
	@:overload(function(type:cs.system.Type):Void {})
	function new(typeName:String, propertyName:String):Void;
}
