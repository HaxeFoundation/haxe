package cs.system.runtime.serialization.formatters;

/** Allows access to field names and field types of objects that support the  interface. */
@:native("System.Runtime.Serialization.Formatters.IFieldInfo")
extern interface IFieldInfo {
	/**
	 * Gets or sets the field names of serialized objects.
	 * @return The field names of serialized objects.
	 */
	var FieldNames(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets the field types of the serialized objects.
	 * @return The field types of the serialized objects.
	 */
	var FieldTypes(default, default):cs.NativeArray<cs.system.Type>;
}
