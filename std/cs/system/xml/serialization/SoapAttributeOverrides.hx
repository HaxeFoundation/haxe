package cs.system.xml.serialization;

/** Allows you to override attributes applied to properties, fields, and classes when you use an  to serialize or deserialize an object as encoded SOAP. */
@:native("System.Xml.Serialization.SoapAttributeOverrides")
extern class SoapAttributeOverrides {
	@:overload(function(index0:cs.system.Type):cs.system.xml.serialization.SoapAttributes {})
	@:native("get_Item")
	function get_Item(index0:cs.system.Type, index1:String):cs.system.xml.serialization.SoapAttributes;
	function new():Void;
	@:overload(function(type:cs.system.Type, attributes:cs.system.xml.serialization.SoapAttributes):Void {})
	/**
	 * Adds a  to the collection of  objects contained by the . The  parameter
	 * specifies the object to be overridden by the . The  parameter specifies the name
	 * of a member that is overridden.
	 * @param type The  of the object to override.
	 * @param member The name of the member to override.
	 * @param attributes A  that represents the overriding attributes.
	 */
	function Add(type:cs.system.Type, member:String, attributes:cs.system.xml.serialization.SoapAttributes):Void;
}
