package cs.system.xml.linq;

/** Represents a name of an XML element or attribute. */
@:native("System.Xml.Linq.XName")
extern class XName {
	/**
	 * Gets the local (unqualified) part of the name.
	 * @return A  that contains the local (unqualified) part of the name.
	 */
	var LocalName(default, never):String;
	/**
	 * Gets the namespace part of the fully qualified name.
	 * @return An  that contains the namespace part of the name.
	 */
	var Namespace(default, never):cs.system.xml.linq.XNamespace;
	/**
	 * Returns the URI of the  for this .
	 * @return The URI of the  for this .
	 */
	var NamespaceName(default, never):String;
	@:overload(function(expandedName:String):cs.system.xml.linq.XName {})
	/**
	 * Gets an  object from an expanded name.
	 * @param expandedName A  that contains an expanded XML name in the format
	 * {namespace}localname.
	 * @return An  object constructed from the expanded name.
	 */
	static function Get(localName:String, namespaceName:String):cs.system.xml.linq.XName;
	/**
	 * Returns a value indicating whether two instances of  are equal.
	 * @param left The first  to compare.
	 * @param right The second  to compare.
	 * @return if  and  are equal; otherwise .
	 */
	static function op_Equality(left:cs.system.xml.linq.XName, right:cs.system.xml.linq.XName):Bool;
	/**
	 * Converts a string formatted as an expanded XML name (that
	 * is,{namespace}localname) to an  object.
	 * @param expandedName A string that contains an expanded XML name in the format
	 * {namespace}localname.
	 * @return An  object constructed from the expanded name.
	 */
	static function op_Implicit(expandedName:String):cs.system.xml.linq.XName;
	/**
	 * Returns a value indicating whether two instances of  are not equal.
	 * @param left The first  to compare.
	 * @param right The second  to compare.
	 * @return if  and  are not equal; otherwise .
	 */
	static function op_Inequality(left:cs.system.xml.linq.XName, right:cs.system.xml.linq.XName):Bool;
	/**
	 * Determines whether the specified  is equal to this .
	 * @param obj The  to compare to the current .
	 * @return if the specified  is equal to the current ; otherwise .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Gets a hash code for this .
	 * @return An  that contains the hash code for the .
	 */
	function GetHashCode():Int;
	/**
	 * Returns the expanded XML name in the format {namespace}localname.
	 * @return A  that contains the expanded XML name in the format
	 * {namespace}localname.
	 */
	function ToString():String;
}
