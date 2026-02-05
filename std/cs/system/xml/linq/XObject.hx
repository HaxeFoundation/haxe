package cs.system.xml.linq;

/** Represents a node or an attribute in an XML tree. */
@:native("System.Xml.Linq.XObject")
extern class XObject {
	/**
	 * Gets the base URI for this .
	 * @return A  that contains the base URI for this .
	 */
	var BaseUri(default, never):String;
	/**
	 * Gets the  for this .
	 * @return The  for this .
	 */
	var Document(default, never):cs.system.xml.linq.XDocument;
	/**
	 * Gets the node type for this .
	 * @return The node type for this .
	 */
	var NodeType(default, never):cs.system.xml.XmlNodeType;
	/**
	 * Gets the parent  of this .
	 * @return The parent  of this .
	 */
	var Parent(default, never):cs.system.xml.linq.XElement;
	/**
	 * Adds an object to the annotation list of this .
	 * @param annotation An object that contains the annotation to add.
	 */
	function AddAnnotation(annotation:Dynamic):Void;
	@:overload(function<T>():T {})
	/**
	 * Gets the first annotation object of the specified type from this .
	 * @param type The type of the annotation to retrieve.
	 * @return The  that contains the first annotation object that matches the
	 * specified type, or  if no annotation is of the specified type.
	 */
	function Annotation(type:cs.system.Type):Dynamic;
	@:overload(function<T>():cs.system.collections.generic.IEnumerable<T> {})
	/**
	 * Gets a collection of annotations of the specified type for this .
	 * @param type The type of the annotations to retrieve.
	 * @return An  of  that contains the annotations that match the specified type for
	 * this .
	 */
	function Annotations(type:cs.system.Type):cs.system.collections.generic.IEnumerable<Dynamic>;
	@:overload(function<T>():Void {})
	/**
	 * Removes the annotations of the specified type from this .
	 * @param type The type of annotations to remove.
	 */
	function RemoveAnnotations(type:cs.system.Type):Void;
}
