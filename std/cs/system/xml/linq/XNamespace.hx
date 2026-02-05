package cs.system.xml.linq;

/** Represents an XML namespace. This class cannot be inherited. */
@:native("System.Xml.Linq.XNamespace")
extern class XNamespace {
	/**
	 * Gets the  object that corresponds to no namespace.
	 * @return The  that corresponds to no namespace.
	 */
	static var None(default, never):cs.system.xml.linq.XNamespace;
	/**
	 * Gets the  object that corresponds to the XML URI ().
	 * @return The  that corresponds to the XML URI ().
	 */
	static var Xml(default, never):cs.system.xml.linq.XNamespace;
	/**
	 * Gets the  object that corresponds to the xmlns URI ().
	 * @return The  that corresponds to the xmlns URI ().
	 */
	static var Xmlns(default, never):cs.system.xml.linq.XNamespace;
	/**
	 * Gets the Uniform Resource Identifier (URI) of this namespace.
	 * @return A  that contains the URI of the namespace.
	 */
	var NamespaceName(default, never):String;
	/**
	 * Gets an  for the specified Uniform Resource Identifier (URI).
	 * @param namespaceName A  that contains a namespace URI.
	 * @return An  created from the specified URI.
	 */
	static function Get(namespaceName:String):cs.system.xml.linq.XNamespace;
	/**
	 * Combines an  object with a local name to create an .
	 * @param ns An  that contains the namespace.
	 * @param localName A  that contains the local name.
	 * @return The new  constructed from the namespace and local name.
	 */
	static function op_Addition(ns:cs.system.xml.linq.XNamespace, localName:String):cs.system.xml.linq.XName;
	/**
	 * Returns a value indicating whether two instances of  are equal.
	 * @param left The first  to compare.
	 * @param right The second  to compare.
	 * @return A  that indicates whether  and  are equal.
	 */
	static function op_Equality(left:cs.system.xml.linq.XNamespace, right:cs.system.xml.linq.XNamespace):Bool;
	/**
	 * Converts a string containing a Uniform Resource Identifier (URI) to an .
	 * @param namespaceName A  that contains the namespace URI.
	 * @return An  constructed from the URI string.
	 */
	static function op_Implicit(namespaceName:String):cs.system.xml.linq.XNamespace;
	/**
	 * Returns a value indicating whether two instances of  are not equal.
	 * @param left The first  to compare.
	 * @param right The second  to compare.
	 * @return A  that indicates whether  and  are not equal.
	 */
	static function op_Inequality(left:cs.system.xml.linq.XNamespace, right:cs.system.xml.linq.XNamespace):Bool;
	/**
	 * Determines whether the specified  is equal to the current .
	 * @param obj The  to compare to the current .
	 * @return A  that indicates whether the specified  is equal to the current .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Gets a hash code for this .
	 * @return An  that contains the hash code for the .
	 */
	function GetHashCode():Int;
	/**
	 * Returns an  object created from this  and the specified local name.
	 * @param localName A  that contains a local name.
	 * @return An  created from this  and the specified local name.
	 */
	function GetName(localName:String):cs.system.xml.linq.XName;
	/**
	 * Returns the URI of this .
	 * @return The URI of this .
	 */
	function ToString():String;
}
