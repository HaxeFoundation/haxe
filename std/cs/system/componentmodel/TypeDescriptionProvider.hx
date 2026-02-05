package cs.system.componentmodel;

/** Provides supplemental metadata to the . */
@:native("System.ComponentModel.TypeDescriptionProvider")
extern class TypeDescriptionProvider {
	/**
	 * Creates an object that can substitute for another data type.
	 * @param provider An optional service provider.
	 * @param objectType The type of object to create. This parameter is never .
	 * @param argTypes An optional array of types that represent the parameter types to
	 * be passed to the object's constructor. This array can be  or of zero length.
	 * @param args An optional array of parameter values to pass to the object's
	 * constructor.
	 * @return The substitute .
	 */
	function CreateInstance(provider:cs.system.IServiceProvider, objectType:cs.system.Type, argTypes:cs.NativeArray<cs.system.Type>, args:cs.NativeArray<Dynamic>):Dynamic;
	/**
	 * Gets a per-object cache, accessed as an  of key/value pairs.
	 * @param instance The object for which to get the cache.
	 * @return An  if the provided object supports caching; otherwise, .
	 */
	function GetCache(instance:Dynamic):cs.system.collections.IDictionary;
	/**
	 * Gets an extended custom type descriptor for the given object.
	 * @param instance The object for which to get the extended type descriptor.
	 * @return An  that can provide extended metadata for the object.
	 */
	function GetExtendedTypeDescriptor(instance:Dynamic):cs.system.componentmodel.ICustomTypeDescriptor;
	/**
	 * Gets the name of the specified component, or  if the component has no name.
	 * @param component The specified component.
	 * @return The name of the specified component.
	 */
	function GetFullComponentName(component:Dynamic):String;
	@:overload(function(instance:Dynamic):cs.system.Type {})
	@:overload(function(objectType:cs.system.Type):cs.system.Type {})
	/**
	 * Performs normal reflection against the given object.
	 * @param instance An instance of the type (should not be ).
	 * @return The type of reflection for this .
	 */
	function GetReflectionType(objectType:cs.system.Type, instance:Dynamic):cs.system.Type;
	/**
	 * Converts a reflection type into a runtime type.
	 * @param reflectionType The type to convert to its runtime equivalent.
	 * @return A  that represents the runtime equivalent of .
	 */
	function GetRuntimeType(reflectionType:cs.system.Type):cs.system.Type;
	@:overload(function(instance:Dynamic):cs.system.componentmodel.ICustomTypeDescriptor {})
	@:overload(function(objectType:cs.system.Type):cs.system.componentmodel.ICustomTypeDescriptor {})
	/**
	 * Gets a custom type descriptor for the given object.
	 * @param instance An instance of the type. Can be  if no instance was passed to
	 * the .
	 * @return An  that can provide metadata for the type.
	 */
	function GetTypeDescriptor(objectType:cs.system.Type, instance:Dynamic):cs.system.componentmodel.ICustomTypeDescriptor;
	/**
	 * Gets a value that indicates whether the specified type is compatible with the
	 * type description and its chain of type description providers.
	 * @param type The type to test for compatibility.
	 * @return if  is compatible with the type description and its chain of type
	 * description providers; otherwise, .
	 */
	function IsSupportedType(type:cs.system.Type):Bool;
}
