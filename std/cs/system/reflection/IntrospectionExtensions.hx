package cs.system.reflection;

/** Contains methods for converting  objects. */
@:native("System.Reflection.IntrospectionExtensions")
extern class IntrospectionExtensions {
	/**
	 * Returns the  representation of the specified type.
	 * @param type The type to convert.
	 * @return The converted object.
	 */
	static function GetTypeInfo(type:cs.system.Type):cs.system.reflection.TypeInfo;
}
