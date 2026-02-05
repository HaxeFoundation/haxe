package cs.system.xml;

/** Represents an XML qualified name. */
@:native("System.Xml.XmlQualifiedName")
extern class XmlQualifiedName {
	/** Provides an empty . */
	static var Empty(default, never):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets a value indicating whether the  is empty.
	 * @return if name and namespace are empty strings; otherwise, .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets a string representation of the qualified name of the .
	 * @return A string representation of the qualified name or String.Empty if a name
	 * is not defined for the object.
	 */
	var Name(default, never):String;
	/**
	 * Gets a string representation of the namespace of the .
	 * @return A string representation of the namespace or String.Empty if a namespace
	 * is not defined for the object.
	 */
	var Namespace(default, never):String;
	@:overload(function():Void {})
	@:overload(function(name:String):Void {})
	function new(name:String, ns:String):Void;
	/**
	 * Compares two  objects.
	 * @param a An  to compare.
	 * @param b An  to compare.
	 * @return if the two objects have the same name and namespace values; otherwise, .
	 */
	static function op_Equality(a:cs.system.xml.XmlQualifiedName, b:cs.system.xml.XmlQualifiedName):Bool;
	/**
	 * Compares two  objects.
	 * @param a An  to compare.
	 * @param b An  to compare.
	 * @return if the name and namespace values for the two objects differ; otherwise,
	 * .
	 */
	static function op_Inequality(a:cs.system.xml.XmlQualifiedName, b:cs.system.xml.XmlQualifiedName):Bool;
	/**
	 * Returns the string value of the .
	 * @return The string value of the  in the format of . If the object does not have
	 * a namespace defined, this method returns just the local name.
	 */
	static function ToString(name:String, ns:String):String;
	/**
	 * Determines whether the specified  object is equal to the current  object.
	 * @param other The  to compare.
	 * @return if the two are the same instance object; otherwise, .
	 */
	function Equals(other:Dynamic):Bool;
	/**
	 * Returns the hash code for the .
	 * @return A hash code for this object.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string value of the .
	 * @return The string value of the  in the format of . If the object does not have
	 * a namespace defined, this method returns just the local name.
	 */
	function ToString():String;
}
