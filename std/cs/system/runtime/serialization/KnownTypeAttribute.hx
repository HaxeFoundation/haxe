package cs.system.runtime.serialization;

/** Specifies types that should be recognized by the  when serializing or deserializing a given type. */
@:native("System.Runtime.Serialization.KnownTypeAttribute")
extern class KnownTypeAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of a method that will return a list of types that should be
	 * recognized during serialization or deserialization.
	 * @return A  that contains the name of the method on the type defined by the 
	 * class.
	 */
	var MethodName(default, never):String;
	/**
	 * Gets the type that should be recognized during serialization or deserialization
	 * by the .
	 * @return The  that is used during serialization or deserialization.
	 */
	var Type(default, never):cs.system.Type;
	@:overload(function(methodName:String):Void {})
	function new(type:cs.system.Type):Void;
}
