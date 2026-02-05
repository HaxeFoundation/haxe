package cs.system.runtime.serialization;

/** When given a class representing a data contract, and metadata representing a member of the contract, produces an XPath query for the member. */
@:native("System.Runtime.Serialization.XPathQueryGenerator")
extern class XPathQueryGenerator {
	@:overload(function(type:cs.system.Type, pathToMember:cs.NativeArray<cs.system.reflection.MemberInfo>, namespaces:cs.Ref<cs.system.xml.XmlNamespaceManager>):String {})
	/**
	 * Creates an XPath from a data contract using the specified contract data type,
	 * array of metadata elements, the top level element, and namespaces.
	 * @param type The type that represents a data contract.
	 * @param pathToMember The metadata, generated using the  method of the  class,
	 * that points to the specific data member used to generate the query.
	 * @param rootElementXpath The top level element in the xpath.
	 * @param namespaces The XML namespaces and their prefixes found in the data
	 * contract.
	 * @return The XPath generated from the type and member data.
	 */
	static function CreateFromDataContractSerializer(type:cs.system.Type, pathToMember:cs.NativeArray<cs.system.reflection.MemberInfo>, rootElementXpath:cs.system.text.StringBuilder, namespaces:cs.Ref<cs.system.xml.XmlNamespaceManager>):String;
}
