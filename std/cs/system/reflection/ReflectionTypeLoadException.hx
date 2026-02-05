package cs.system.reflection;

/** The exception that is thrown by the  method if any of the classes in a module cannot be loaded. This class cannot be inherited. */
@:native("System.Reflection.ReflectionTypeLoadException")
extern class ReflectionTypeLoadException extends cs.system.SystemException {
	/**
	 * Gets the array of exceptions thrown by the class loader.
	 * @return An array of type  containing the exceptions thrown by the class loader.
	 * The null values in the  array of this instance line up with the exceptions in
	 * this array.
	 */
	var LoaderExceptions(default, never):cs.NativeArray<cs.system.Exception>;
	/**
	 * Gets the array of classes that were defined in the module and loaded.
	 * @return An array of type  containing the classes that were defined in the module
	 * and loaded. This array can contain some  values.
	 */
	var Types(default, never):cs.NativeArray<cs.system.Type>;
	@:overload(function(classes:cs.NativeArray<cs.system.Type>, exceptions:cs.NativeArray<cs.system.Exception>):Void {})
	function new(classes:cs.NativeArray<cs.system.Type>, exceptions:cs.NativeArray<cs.system.Exception>, message:String):Void;
	/**
	 * Provides an  implementation for serialized objects.
	 * @param info The information and data needed to serialize or deserialize an
	 * object.
	 * @param context The context for the serialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
