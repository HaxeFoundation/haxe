package cs.system.reflection.emit;

/** Defines and represents a module in a dynamic assembly. */
@:native("System.Reflection.Emit.ModuleBuilder")
extern class ModuleBuilder extends cs.system.reflection.Module {
	/** Completes the global function definitions and global data definitions for this dynamic module. */
	function CreateGlobalFunctions():Void;
	/**
	 * Defines an enumeration type that is a value type with a single non-static field
	 * called  of the specified type.
	 * @param name The full path of the enumeration type.  cannot contain embedded
	 * nulls.
	 * @param visibility The type attributes for the enumeration. The attributes are
	 * any bits defined by .
	 * @param underlyingType The underlying type for the enumeration. This must be a
	 * built-in integer type.
	 * @return The defined enumeration.
	 */
	function DefineEnum(name:String, visibility:cs.system.reflection.TypeAttributes, underlyingType:cs.system.Type):cs.system.reflection.emit.EnumBuilder;
	@:overload(function(name:String, attributes:cs.system.reflection.MethodAttributes, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.MethodBuilder {})
	@:overload(function(name:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.MethodBuilder {})
	/**
	 * Defines a global method with the specified name, attributes, calling convention,
	 * return type, and parameter types.
	 * @param name The name of the method.  cannot contain embedded nulls.
	 * @param attributes The attributes of the method.  must include .
	 * @param callingConvention The calling convention for the method.
	 * @param returnType The return type of the method.
	 * @param parameterTypes The types of the method's parameters.
	 * @return The defined global method.
	 */
	function DefineGlobalMethod(name:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, requiredReturnTypeCustomModifiers:cs.NativeArray<cs.system.Type>, optionalReturnTypeCustomModifiers:cs.NativeArray<cs.system.Type>, parameterTypes:cs.NativeArray<cs.system.Type>, requiredParameterTypeCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, optionalParameterTypeCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):cs.system.reflection.emit.MethodBuilder;
	/**
	 * Defines an initialized data field in the .sdata section of the portable
	 * executable (PE) file.
	 * @param name The name used to refer to the data.  cannot contain embedded nulls.
	 * @param data The binary large object (BLOB) of data.
	 * @param attributes The attributes for the field. The default is .
	 * @return A field to reference the data.
	 */
	function DefineInitializedData(name:String, data:cs.NativeArray<cs.UInt8>, attributes:cs.system.reflection.FieldAttributes):cs.system.reflection.emit.FieldBuilder;
	@:overload(function(name:String, dllName:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, nativeCallConv:cs.system.runtime.interopservices.CallingConvention, nativeCharSet:cs.system.runtime.interopservices.CharSet):cs.system.reflection.emit.MethodBuilder {})
	/**
	 * Defines a  method with the specified name, the name of the DLL in which the
	 * method is defined, the attributes of the method, the calling convention of the
	 * method, the return type of the method, the types of the parameters of the
	 * method, and the  flags.
	 * @param name The name of the  method.  cannot contain embedded nulls.
	 * @param dllName The name of the DLL in which the  method is defined.
	 * @param attributes The attributes of the method.
	 * @param callingConvention The method's calling convention.
	 * @param returnType The method's return type.
	 * @param parameterTypes The types of the method's parameters.
	 * @param nativeCallConv The native calling convention.
	 * @param nativeCharSet The method's native character set.
	 * @return The defined  method.
	 */
	function DefinePInvokeMethod(name:String, dllName:String, entryName:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, nativeCallConv:cs.system.runtime.interopservices.CallingConvention, nativeCharSet:cs.system.runtime.interopservices.CharSet):cs.system.reflection.emit.MethodBuilder;
	@:overload(function(name:String):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type, typesize:Int):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type, packsize:cs.system.reflection.emit.PackingSize):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type, interfaces:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.TypeBuilder {})
	/**
	 * Constructs a  for a private type with the specified name in this module.
	 * @param name The full path of the type, including the namespace.  cannot contain
	 * embedded nulls.
	 * @return A private type with the specified name.
	 */
	function DefineType(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type, packingSize:cs.system.reflection.emit.PackingSize, typesize:Int):cs.system.reflection.emit.TypeBuilder;
	/**
	 * Defines an uninitialized data field in the .sdata section of the portable
	 * executable (PE) file.
	 * @param name The name used to refer to the data.  cannot contain embedded nulls.
	 * @param size The size of the data field.
	 * @param attributes The attributes for the field.
	 * @return A field to reference the data.
	 */
	function DefineUninitializedData(name:String, size:Int, attributes:cs.system.reflection.FieldAttributes):cs.system.reflection.emit.FieldBuilder;
	/**
	 * Returns a value that indicates whether this instance is equal to the specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the named method on an array class.
	 * @param arrayClass An array class.
	 * @param methodName The name of a method on the array class.
	 * @param callingConvention The method's calling convention.
	 * @param returnType The return type of the method.
	 * @param parameterTypes The types of the method's parameters.
	 * @return The named method on an array class.
	 */
	function GetArrayMethod(arrayClass:cs.system.Type, methodName:String, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.MethodInfo;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Applies a custom attribute to this module by using a specified binary large
	 * object (BLOB) that represents the attribute.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte BLOB representing the attribute.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
}
