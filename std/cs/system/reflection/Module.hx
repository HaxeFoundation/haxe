package cs.system.reflection;

/** Performs reflection on a module. */
@:native("System.Reflection.Module")
extern class Module {
	/** A  object that filters the list of types defined in this module based upon the name. This field is case-sensitive and read-only. */
	static var FilterTypeName(default, never):cs.system.reflection.TypeFilter;
	/** A  object that filters the list of types defined in this module based upon the name. This field is case-insensitive and read-only. */
	static var FilterTypeNameIgnoreCase(default, never):cs.system.reflection.TypeFilter;
	/**
	 * Gets the appropriate  for this instance of .
	 * @return An  object.
	 */
	var Assembly(default, never):cs.system.reflection.Assembly;
	/**
	 * Gets a collection that contains this module's custom attributes.
	 * @return A collection that contains this module's custom attributes.
	 */
	var CustomAttributes(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.CustomAttributeData>;
	/**
	 * Gets a string representing the fully qualified name and path to this module.
	 * @return The fully qualified module name.
	 */
	var FullyQualifiedName(default, never):String;
	/**
	 * Gets the metadata stream version.
	 * @return A 32-bit integer representing the metadata stream version. The
	 * high-order two bytes represent the major version number, and the low-order two
	 * bytes represent the minor version number.
	 */
	var MDStreamVersion(default, never):Int;
	/**
	 * Gets a token that identifies the module in metadata.
	 * @return An integer token that identifies the current module in metadata.
	 */
	var MetadataToken(default, never):Int;
	/**
	 * Gets a handle for the module.
	 * @return A  structure for the current module.
	 */
	var ModuleHandle(default, never):cs.system.ModuleHandle;
	/**
	 * Gets a universally unique identifier (UUID) that can be used to distinguish
	 * between two versions of a module.
	 * @return A  that can be used to distinguish between two versions of a module.
	 */
	var ModuleVersionId(default, never):cs.system.Guid;
	/**
	 * Gets a  representing the name of the module with the path removed.
	 * @return The module name with no path.
	 */
	var Name(default, never):String;
	/**
	 * Gets a string representing the name of the module.
	 * @return The module name.
	 */
	var ScopeName(default, never):String;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.Module, right:cs.system.reflection.Module):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.Module, right:cs.system.reflection.Module):Bool;
	/**
	 * Determines whether this module and the specified object are equal.
	 * @param o The object to compare with this instance.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	/**
	 * Returns an array of classes accepted by the given filter and filter criteria.
	 * @param filter The delegate used to filter the classes.
	 * @param filterCriteria An Object used to filter the classes.
	 * @return An array of type  containing classes that were accepted by the filter.
	 */
	function FindTypes(filter:cs.system.reflection.TypeFilter, filterCriteria:Dynamic):cs.NativeArray<cs.system.Type>;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns all custom attributes.
	 * @param inherit This argument is ignored for objects of this type.
	 * @return An array of type  containing all custom attributes.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Returns a list of  objects for the current module, which can be used in the
	 * reflection-only context.
	 * @return A generic list of  objects representing data about the attributes that
	 * have been applied to the current module.
	 */
	function GetCustomAttributesData():cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeData>;
	@:overload(function(name:String):cs.system.reflection.FieldInfo {})
	/**
	 * Returns a field having the specified name.
	 * @param name The field name.
	 * @return A  object having the specified name, or  if the field does not exist.
	 */
	function GetField(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.FieldInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.FieldInfo> {})
	/**
	 * Returns the global fields defined on the module.
	 * @return An array of  objects representing the global fields defined on the
	 * module; if there are no global fields, an empty array is returned.
	 */
	function GetFields(bindingFlags:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.FieldInfo>;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function(name:String):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, types:cs.NativeArray<cs.system.Type>):cs.system.reflection.MethodInfo {})
	/**
	 * Returns a method having the specified name.
	 * @param name The method name.
	 * @return A  object having the specified name, or  if the method does not exist.
	 */
	function GetMethod(name:String, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, callConvention:cs.system.reflection.CallingConventions, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.MethodInfo> {})
	/**
	 * Returns the global methods defined on the module.
	 * @return An array of  objects representing all the global methods defined on the
	 * module; if there are no global methods, an empty array is returned.
	 */
	function GetMethods(bindingFlags:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MethodInfo>;
	/**
	 * Provides an  implementation for serialized objects.
	 * @param info The information and data needed to serialize or deserialize an
	 * object.
	 * @param context The context for the serialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Gets a pair of values indicating the nature of the code in a module and the
	 * platform targeted by the module.
	 * @param peKind When this method returns, a combination of the  values indicating
	 * the nature of the code in the module.
	 * @param machine When this method returns, one of the  values indicating the
	 * platform targeted by the module.
	 */
	function GetPEKind(peKind:cs.Ref<cs.system.reflection.PortableExecutableKinds>, machine:cs.Ref<cs.system.reflection.ImageFileMachine>):Void;
	@:overload(function(className:String):cs.system.Type {})
	@:overload(function(className:String, ignoreCase:Bool):cs.system.Type {})
	/**
	 * Returns the specified type, performing a case-sensitive search.
	 * @param className The name of the type to locate. The name must be fully
	 * qualified with the namespace.
	 * @return A  object representing the given type, if the type is in this module;
	 * otherwise, .
	 */
	function GetType(className:String, throwOnError:Bool, ignoreCase:Bool):cs.system.Type;
	/**
	 * Returns all the types defined within this module.
	 * @return An array of type  containing types defined within the module that is
	 * reflected by this instance.
	 */
	function GetTypes():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns a value that indicates whether the specified attribute type has been
	 * applied to this module.
	 * @param attributeType The type of custom attribute to test for.
	 * @param inherit This argument is ignored for objects of this type.
	 * @return if one or more instances of  have been applied to this module;
	 * otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Gets a value indicating whether the object is a resource.
	 * @return if the object is a resource; otherwise, .
	 */
	function IsResource():Bool;
	@:overload(function(metadataToken:Int):cs.system.reflection.FieldInfo {})
	/**
	 * Returns the field identified by the specified metadata token.
	 * @param metadataToken A metadata token that identifies a field in the module.
	 * @return A  object representing the field that is identified by the specified
	 * metadata token.
	 */
	function ResolveField(metadataToken:Int, genericTypeArguments:cs.NativeArray<cs.system.Type>, genericMethodArguments:cs.NativeArray<cs.system.Type>):cs.system.reflection.FieldInfo;
	@:overload(function(metadataToken:Int):cs.system.reflection.MemberInfo {})
	/**
	 * Returns the type or member identified by the specified metadata token.
	 * @param metadataToken A metadata token that identifies a type or member in the
	 * module.
	 * @return A  object representing the type or member that is identified by the
	 * specified metadata token.
	 */
	function ResolveMember(metadataToken:Int, genericTypeArguments:cs.NativeArray<cs.system.Type>, genericMethodArguments:cs.NativeArray<cs.system.Type>):cs.system.reflection.MemberInfo;
	@:overload(function(metadataToken:Int):cs.system.reflection.MethodBase {})
	/**
	 * Returns the method or constructor identified by the specified metadata token.
	 * @param metadataToken A metadata token that identifies a method or constructor in
	 * the module.
	 * @return A  object representing the method or constructor that is identified by
	 * the specified metadata token.
	 */
	function ResolveMethod(metadataToken:Int, genericTypeArguments:cs.NativeArray<cs.system.Type>, genericMethodArguments:cs.NativeArray<cs.system.Type>):cs.system.reflection.MethodBase;
	/**
	 * Returns the signature blob identified by a metadata token.
	 * @param metadataToken A metadata token that identifies a signature in the module.
	 * @return An array of bytes representing the signature blob.
	 */
	function ResolveSignature(metadataToken:Int):cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the string identified by the specified metadata token.
	 * @param metadataToken A metadata token that identifies a string in the string
	 * heap of the module.
	 * @return A  containing a string value from the metadata string heap.
	 */
	function ResolveString(metadataToken:Int):String;
	@:overload(function(metadataToken:Int):cs.system.Type {})
	/**
	 * Returns the type identified by the specified metadata token.
	 * @param metadataToken A metadata token that identifies a type in the module.
	 * @return A  object representing the type that is identified by the specified
	 * metadata token.
	 */
	function ResolveType(metadataToken:Int, genericTypeArguments:cs.NativeArray<cs.system.Type>, genericMethodArguments:cs.NativeArray<cs.system.Type>):cs.system.Type;
	/**
	 * Returns the name of the module.
	 * @return A  representing the name of this module.
	 */
	function ToString():String;
}
