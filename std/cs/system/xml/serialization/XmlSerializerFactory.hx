package cs.system.xml.serialization;

/** Creates typed versions of the  for more efficient serialization. */
@:native("System.Xml.Serialization.XmlSerializerFactory")
extern class XmlSerializerFactory {
	function new():Void;
	@:overload(function(type:cs.system.Type):cs.system.xml.serialization.XmlSerializer {})
	@:overload(function(xmlTypeMapping:cs.system.xml.serialization.XmlTypeMapping):cs.system.xml.serialization.XmlSerializer {})
	@:overload(function(type:cs.system.Type, defaultNamespace:String):cs.system.xml.serialization.XmlSerializer {})
	@:overload(function(type:cs.system.Type, extraTypes:cs.NativeArray<cs.system.Type>):cs.system.xml.serialization.XmlSerializer {})
	@:overload(function(type:cs.system.Type, overrides:cs.system.xml.serialization.XmlAttributeOverrides):cs.system.xml.serialization.XmlSerializer {})
	@:overload(function(type:cs.system.Type, root:cs.system.xml.serialization.XmlRootAttribute):cs.system.xml.serialization.XmlSerializer {})
	@:overload(function(type:cs.system.Type, overrides:cs.system.xml.serialization.XmlAttributeOverrides, extraTypes:cs.NativeArray<cs.system.Type>, root:cs.system.xml.serialization.XmlRootAttribute, defaultNamespace:String):cs.system.xml.serialization.XmlSerializer {})
	/**
	 * Returns a derivation of the  class that is used to serialize the specified type.
	 * @param type The  to serialize.
	 * @return A derivation of the  class that is specifically created to serialize the
	 * specified type.
	 */
	function CreateSerializer(type:cs.system.Type, overrides:cs.system.xml.serialization.XmlAttributeOverrides, extraTypes:cs.NativeArray<cs.system.Type>, root:cs.system.xml.serialization.XmlRootAttribute, defaultNamespace:String, location:String):cs.system.xml.serialization.XmlSerializer;
}
