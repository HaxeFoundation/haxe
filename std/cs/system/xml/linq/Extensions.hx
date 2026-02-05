package cs.system.xml.linq;

/** Contains the LINQ to XML extension methods. */
@:native("System.Xml.Linq.Extensions")
extern class Extensions {
	@:overload(function<T>(source:cs.system.collections.generic.IEnumerable<T>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of elements that contains the ancestors of every node in
	 * the source collection.
	 * @param T The type of the objects in , constrained to .
	 * @param source An  of  that contains the source collection.
	 * @return An  of  that contains the ancestors of every node in the source
	 * collection.
	 */
	static function Ancestors<T>(source:cs.system.collections.generic.IEnumerable<T>, name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	@:overload(function(source:cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of elements that contains every element in the source
	 * collection, and the ancestors of every element in the source collection.
	 * @param source An  of  that contains the source collection.
	 * @return An  of  that contains every element in the source collection, and the
	 * ancestors of every element in the source collection.
	 */
	static function AncestorsAndSelf(source:cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>, name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	@:overload(function(source:cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XAttribute> {})
	/**
	 * Returns a collection of the attributes of every element in the source
	 * collection.
	 * @param source An  of  that contains the source collection.
	 * @return An  of  that contains the attributes of every element in the source
	 * collection.
	 */
	static function Attributes(source:cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>, name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XAttribute>;
	/**
	 * Returns a collection of the descendant nodes of every document and element in
	 * the source collection.
	 * @param T The type of the objects in , constrained to .
	 * @param source An  of  that contains the source collection.
	 * @return An  of  of the descendant nodes of every document and element in the
	 * source collection.
	 */
	static function DescendantNodes<T>(source:cs.system.collections.generic.IEnumerable<T>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XNode>;
	/**
	 * Returns a collection of nodes that contains every element in the source
	 * collection, and the descendant nodes of every element in the source collection.
	 * @param source An  of  that contains the source collection.
	 * @return An  of  that contains every element in the source collection, and the
	 * descendant nodes of every element in the source collection.
	 */
	static function DescendantNodesAndSelf(source:cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XNode>;
	@:overload(function<T>(source:cs.system.collections.generic.IEnumerable<T>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of elements that contains the descendant elements of every
	 * element and document in the source collection.
	 * @param T The type of the objects in , constrained to .
	 * @param source An  of  that contains the source collection.
	 * @return An  of  that contains the descendant elements of every element and
	 * document in the source collection.
	 */
	static function Descendants<T>(source:cs.system.collections.generic.IEnumerable<T>, name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	@:overload(function(source:cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of elements that contains every element in the source
	 * collection, and the descendent elements of every element in the source
	 * collection.
	 * @param source An  of  that contains the source collection.
	 * @return An  of  that contains every element in the source collection, and the
	 * descendent elements of every element in the source collection.
	 */
	static function DescendantsAndSelf(source:cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>, name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	@:overload(function<T>(source:cs.system.collections.generic.IEnumerable<T>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Returns a collection of the child elements of every element and document in the
	 * source collection.
	 * @param T The type of the objects in , constrained to .
	 * @param source An  of  that contains the source collection.
	 * @return An  of  of the child elements of every element or document in the source
	 * collection.
	 */
	static function Elements<T>(source:cs.system.collections.generic.IEnumerable<T>, name:cs.system.xml.linq.XName):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
	/**
	 * Returns a collection of nodes that contains all nodes in the source collection,
	 * sorted in document order.
	 * @param T The type of the objects in , constrained to .
	 * @param source An  of  that contains the source collection.
	 * @return An  of  that contains all nodes in the source collection, sorted in
	 * document order.
	 */
	static function InDocumentOrder<T>(source:cs.system.collections.generic.IEnumerable<T>):cs.system.collections.generic.IEnumerable<T>;
	/**
	 * Returns a collection of the child nodes of every document and element in the
	 * source collection.
	 * @param T The type of the objects in , constrained to .
	 * @param source An  of  that contains the source collection.
	 * @return An  of  of the child nodes of every document and element in the source
	 * collection.
	 */
	static function Nodes<T>(source:cs.system.collections.generic.IEnumerable<T>):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XNode>;
	@:overload(function(source:cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XAttribute>):Void {})
	/**
	 * Removes every attribute in the source collection from its parent element.
	 * @param source An  of  that contains the source collection.
	 */
	static function Remove<T>(source:cs.system.collections.generic.IEnumerable<T>):Void;
}
