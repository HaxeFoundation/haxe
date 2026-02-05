package cs.system.runtime.serialization;

/** Allows users to control class loading and mandate what class to load. */
@:native("System.Runtime.Serialization.SerializationBinder")
extern class SerializationBinder {
	/**
	 * When overridden in a derived class, controls the binding of a serialized object
	 * to a type.
	 * @param serializedType The type of the object the formatter creates a new
	 * instance of.
	 * @param assemblyName Specifies the  name of the serialized object.
	 * @param typeName Specifies the  name of the serialized object.
	 */
	function BindToName(serializedType:cs.system.Type, assemblyName:cs.Ref<String>, typeName:cs.Ref<String>):Void;
	/**
	 * When overridden in a derived class, controls the binding of a serialized object
	 * to a type.
	 * @param assemblyName Specifies the  name of the serialized object.
	 * @param typeName Specifies the  name of the serialized object.
	 * @return The type of the object the formatter creates a new instance of.
	 */
	function BindToType(assemblyName:String, typeName:String):cs.system.Type;
}
