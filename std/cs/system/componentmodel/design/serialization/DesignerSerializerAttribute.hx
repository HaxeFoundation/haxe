package cs.system.componentmodel.design.serialization;

/** Indicates a serializer for the serialization manager to use to serialize the values of the type this attribute is applied to. This class cannot be inherited. */
@:native("System.ComponentModel.Design.Serialization.DesignerSerializerAttribute")
extern class DesignerSerializerAttribute extends cs.system.Attribute {
	/**
	 * Gets the fully qualified type name of the serializer base type.
	 * @return The fully qualified type name of the serializer base type.
	 */
	var SerializerBaseTypeName(default, never):String;
	/**
	 * Gets the fully qualified type name of the serializer.
	 * @return The fully qualified type name of the serializer.
	 */
	var SerializerTypeName(default, never):String;
	@:overload(function(serializerTypeName:String, baseSerializerTypeName:String):Void {})
	@:overload(function(serializerTypeName:String, baseSerializerType:cs.system.Type):Void {})
	function new(serializerType:cs.system.Type, baseSerializerType:cs.system.Type):Void;
}
