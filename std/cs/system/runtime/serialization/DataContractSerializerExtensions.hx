package cs.system.runtime.serialization;

/** Extends the  class by providing methods for setting and getting an . */
@:native("System.Runtime.Serialization.DataContractSerializerExtensions")
extern class DataContractSerializerExtensions {
	/**
	 * Returns the surrogate serialization provider for this serializer.
	 * @param serializer The serializer which is being surrogated.
	 * @return The surrogate serializer.
	 */
	static function GetSerializationSurrogateProvider(serializer:cs.system.runtime.serialization.DataContractSerializer):cs.system.runtime.serialization.ISerializationSurrogateProvider;
	/**
	 * Specifies a surrogate serialization provider for this .
	 * @param serializer The serializer which is being surrogated.
	 * @param provider The surrogate serialization provider.
	 */
	static function SetSerializationSurrogateProvider(serializer:cs.system.runtime.serialization.DataContractSerializer, provider:cs.system.runtime.serialization.ISerializationSurrogateProvider):Void;
}
