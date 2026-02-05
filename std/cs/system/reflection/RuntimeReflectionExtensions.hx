package cs.system.reflection;

/** Provides methods that retrieve information about types at run time. */
@:native("System.Reflection.RuntimeReflectionExtensions")
extern class RuntimeReflectionExtensions {
	/**
	 * Gets an object that represents the method represented by the specified delegate.
	 * @param del The delegate to examine.
	 * @return An object that represents the method.
	 */
	static function GetMethodInfo(del:cs.system.Delegate):cs.system.reflection.MethodInfo;
	/**
	 * Retrieves an object that represents the specified method on the direct or
	 * indirect base class where the method was first declared.
	 * @param method The method to retrieve information about.
	 * @return An object that represents the specified method's initial declaration on
	 * a base class.
	 */
	static function GetRuntimeBaseDefinition(method:cs.system.reflection.MethodInfo):cs.system.reflection.MethodInfo;
	/**
	 * Retrieves an object that represents the specified event.
	 * @param type The type that contains the event.
	 * @param name The name of the event.
	 * @return An object that represents the specified event, or  if the event is not
	 * found.
	 */
	static function GetRuntimeEvent(type:cs.system.Type, name:String):cs.system.reflection.EventInfo;
	/**
	 * Retrieves a collection that represents all the events defined on a specified
	 * type.
	 * @param type The type that contains the events.
	 * @return A collection of events for the specified type.
	 */
	static function GetRuntimeEvents(type:cs.system.Type):cs.system.collections.generic.IEnumerable<cs.system.reflection.EventInfo>;
	/**
	 * Retrieves an object that represents a specified field.
	 * @param type The type that contains the field.
	 * @param name The name of the field.
	 * @return An object that represents the specified field, or  if the field is not
	 * found.
	 */
	static function GetRuntimeField(type:cs.system.Type, name:String):cs.system.reflection.FieldInfo;
	/**
	 * Retrieves a collection that represents all the fields defined on a specified
	 * type.
	 * @param type The type that contains the fields.
	 * @return A collection of fields for the specified type.
	 */
	static function GetRuntimeFields(type:cs.system.Type):cs.system.collections.generic.IEnumerable<cs.system.reflection.FieldInfo>;
	/**
	 * Returns an interface mapping for the specified type and the specified interface.
	 * @param typeInfo The type to retrieve a mapping for.
	 * @param interfaceType The interface to retrieve a mapping for.
	 * @return An object that represents the interface mapping for the specified
	 * interface and type.
	 */
	static function GetRuntimeInterfaceMap(typeInfo:cs.system.reflection.TypeInfo, interfaceType:cs.system.Type):cs.system.reflection.InterfaceMapping;
	/**
	 * Retrieves an object that represents a specified method.
	 * @param type The type that contains the method.
	 * @param name The name of the method.
	 * @param parameters An array that contains the method's parameters.
	 * @return An object that represents the specified method, or  if the method is not
	 * found.
	 */
	static function GetRuntimeMethod(type:cs.system.Type, name:String, parameters:cs.NativeArray<cs.system.Type>):cs.system.reflection.MethodInfo;
	/**
	 * Retrieves a collection that represents all methods defined on a specified type.
	 * @param type The type that contains the methods.
	 * @return A collection of methods for the specified type.
	 */
	static function GetRuntimeMethods(type:cs.system.Type):cs.system.collections.generic.IEnumerable<cs.system.reflection.MethodInfo>;
	/**
	 * Retrieves a collection that represents all the properties defined on a specified
	 * type.
	 * @param type The type that contains the properties.
	 * @return A collection of properties for the specified type.
	 */
	static function GetRuntimeProperties(type:cs.system.Type):cs.system.collections.generic.IEnumerable<cs.system.reflection.PropertyInfo>;
	/**
	 * Retrieves an object that represents a specified property.
	 * @param type The type that contains the property.
	 * @param name The name of the property.
	 * @return An object that represents the specified property, or  if the property is
	 * not found.
	 */
	static function GetRuntimeProperty(type:cs.system.Type, name:String):cs.system.reflection.PropertyInfo;
}
