package cs.system.xml.serialization;

/** Allows you to override property, field, and class attributes when you use the  to serialize or deserialize an object. */
@:native("System.Xml.Serialization.XmlAttributeOverrides")
extern class XmlAttributeOverrides {
	@:overload(function(index0:cs.system.Type):cs.system.xml.serialization.XmlAttributes {})
	@:native("get_Item")
	function get_Item(index0:cs.system.Type, index1:String):cs.system.xml.serialization.XmlAttributes;
	function new():Void;
	@:overload(function(type:cs.system.Type, attributes:cs.system.xml.serialization.XmlAttributes):Void {})
	/**
	 * Adds an  object to the collection of  objects. The  parameter specifies an
	 * object to be overridden. The  parameter specifies the name of a member that is
	 * overridden.
	 * @param type The  of the object to override.
	 * @param member The name of the member to override.
	 * @param attributes An  object that represents the overriding attributes.
	 */
	function Add(type:cs.system.Type, member:String, attributes:cs.system.xml.serialization.XmlAttributes):Void;
}
