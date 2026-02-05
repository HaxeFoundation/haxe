package cs.system.componentmodel.design.serialization;

/** The  attribute is placed on a serializer to indicate the class to use as a default provider of that type of serializer. */
@:native("System.ComponentModel.Design.Serialization.DefaultSerializationProviderAttribute")
extern class DefaultSerializationProviderAttribute extends cs.system.Attribute {
	/**
	 * Gets the type name of the serialization provider.
	 * @return A string containing the name of the provider.
	 */
	var ProviderTypeName(default, never):String;
	@:overload(function(providerTypeName:String):Void {})
	function new(providerType:cs.system.Type):Void;
}
