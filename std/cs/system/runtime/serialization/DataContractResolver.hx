package cs.system.runtime.serialization;

/** Provides a mechanism for dynamically mapping types to and from  representations during serialization and deserialization. */
@:native("System.Runtime.Serialization.DataContractResolver")
extern class DataContractResolver {
	/**
	 * Override this method to map the specified  name and namespace to a data contract
	 * type during deserialization.
	 * @param typeName The  name to map.
	 * @param typeNamespace The  namespace to map.
	 * @param declaredType The type declared in the data contract.
	 * @param knownTypeResolver The known type resolver.
	 * @return The type the  name and namespace is mapped to.
	 */
	function ResolveName(typeName:String, typeNamespace:String, declaredType:cs.system.Type, knownTypeResolver:cs.system.runtime.serialization.DataContractResolver):cs.system.Type;
	/**
	 * Override this method to map a data contract type to an  name and namespace
	 * during serialization.
	 * @param type The type to map.
	 * @param declaredType The type declared in the data contract.
	 * @param knownTypeResolver The known type resolver.
	 * @param typeName The xsi:type name.
	 * @param typeNamespace The xsi:type namespace.
	 * @return if mapping succeeded; otherwise, .
	 */
	function TryResolveType(type:cs.system.Type, declaredType:cs.system.Type, knownTypeResolver:cs.system.runtime.serialization.DataContractResolver, typeName:cs.Ref<cs.system.xml.XmlDictionaryString>, typeNamespace:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool;
}
