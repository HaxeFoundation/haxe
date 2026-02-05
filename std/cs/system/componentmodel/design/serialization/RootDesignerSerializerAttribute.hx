package cs.system.componentmodel.design.serialization;

/** Indicates the base serializer to use for a root designer object. This class cannot be inherited. */
@:native("System.ComponentModel.Design.Serialization.RootDesignerSerializerAttribute")
extern class RootDesignerSerializerAttribute extends cs.system.Attribute {
	/**
	 * Gets a value indicating whether the root serializer supports reloading of the
	 * design document without first disposing the designer host.
	 * @return if the root serializer supports reloading; otherwise, .
	 */
	var Reloadable(default, never):Bool;
	/**
	 * Gets the fully qualified type name of the base type of the serializer.
	 * @return The name of the base type of the serializer.
	 */
	var SerializerBaseTypeName(default, never):String;
	/**
	 * Gets the fully qualified type name of the serializer.
	 * @return The name of the type of the serializer.
	 */
	var SerializerTypeName(default, never):String;
	@:overload(function(serializerTypeName:String, baseSerializerTypeName:String, reloadable:Bool):Void {})
	@:overload(function(serializerTypeName:String, baseSerializerType:cs.system.Type, reloadable:Bool):Void {})
	function new(serializerType:cs.system.Type, baseSerializerType:cs.system.Type, reloadable:Bool):Void;
}
