package cs.system.xml.serialization;

/** Contains the XML namespaces and prefixes that the  uses to generate qualified names in an XML-document instance. */
@:native("System.Xml.Serialization.XmlSerializerNamespaces")
extern class XmlSerializerNamespaces {
	/**
	 * Gets the number of prefix and namespace pairs in the collection.
	 * @return The number of prefix and namespace pairs in the collection.
	 */
	var Count(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(namespaces:cs.system.xml.serialization.XmlSerializerNamespaces):Void {})
	function new(namespaces:cs.NativeArray<cs.system.xml.XmlQualifiedName>):Void;
	/**
	 * Adds a prefix and namespace pair to an  object.
	 * @param prefix The prefix associated with an XML namespace.
	 * @param ns An XML namespace.
	 */
	function Add(prefix:String, ns:String):Void;
	/**
	 * Gets the array of prefix and namespace pairs in an  object.
	 * @return An array of  objects that are used as qualified names in an XML
	 * document.
	 */
	function ToArray():cs.NativeArray<cs.system.xml.XmlQualifiedName>;
}
