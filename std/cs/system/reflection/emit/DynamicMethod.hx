package cs.system.reflection.emit;

/** Defines and represents a dynamic method that can be compiled, executed, and discarded. Discarded methods are available for garbage collection. */
@:native("System.Reflection.Emit.DynamicMethod")
extern class DynamicMethod extends cs.system.reflection.MethodInfo {
	/**
	 * Gets or sets a value indicating whether the local variables in the method are
	 * zero-initialized.
	 * @return if the local variables in the method are zero-initialized; otherwise, .
	 * The default is .
	 */
	var InitLocals(default, default):Bool;
	@:overload(function(name:String, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):Void {})
	@:overload(function(name:String, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, restrictedSkipVisibility:Bool):Void {})
	@:overload(function(name:String, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, m:cs.system.reflection.Module):Void {})
	@:overload(function(name:String, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, owner:cs.system.Type):Void {})
	@:overload(function(name:String, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, m:cs.system.reflection.Module, skipVisibility:Bool):Void {})
	@:overload(function(name:String, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, owner:cs.system.Type, skipVisibility:Bool):Void {})
	@:overload(function(name:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, m:cs.system.reflection.Module, skipVisibility:Bool):Void {})
	function new(name:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, owner:cs.system.Type, skipVisibility:Bool):Void;
	@:overload(function(delegateType:cs.system.Type):cs.system.Delegate {})
	/**
	 * Completes the dynamic method and creates a delegate that can be used to execute
	 * it.
	 * @param delegateType A delegate type whose signature matches that of the dynamic
	 * method.
	 * @return A delegate of the specified type, which can be used to execute the
	 * dynamic method.
	 */
	function CreateDelegate(delegateType:cs.system.Type, target:Dynamic):cs.system.Delegate;
	/**
	 * Defines a parameter of the dynamic method.
	 * @param position The position of the parameter in the parameter list. Parameters
	 * are indexed beginning with the number 1 for the first parameter.
	 * @param attributes A bitwise combination of  values that specifies the attributes
	 * of the parameter.
	 * @param parameterName The name of the parameter. The name can be a zero-length
	 * string.
	 * @return Always returns .
	 */
	function DefineParameter(position:Int, attributes:cs.system.reflection.ParameterAttributes, parameterName:String):cs.system.reflection.emit.ParameterBuilder;
	/**
	 * Returns the base implementation for the method.
	 * @return The base implementation of the method.
	 */
	function GetBaseDefinition():cs.system.reflection.MethodInfo;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns all the custom attributes defined for the method.
	 * @param inherit to search the method's inheritance chain to find the custom
	 * attributes;  to check only the current method.
	 * @return An array of objects representing all the custom attributes of the
	 * method.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Returns a  object that can be used to generate a method body from metadata
	 * tokens, scopes, and Microsoft intermediate language (MSIL) streams.
	 * @return A  object that can be used to generate a method body from metadata
	 * tokens, scopes, and MSIL streams.
	 */
	function GetDynamicILInfo():cs.system.reflection.emit.DynamicILInfo;
	@:overload(function():cs.system.reflection.emit.ILGenerator {})
	/**
	 * Returns a Microsoft intermediate language (MSIL) generator for the method with a
	 * default MSIL stream size of 64 bytes.
	 * @return An  object for the method.
	 */
	function GetILGenerator(streamSize:Int):cs.system.reflection.emit.ILGenerator;
	/**
	 * Returns the implementation flags for the method.
	 * @return A bitwise combination of  values representing the implementation flags
	 * for the method.
	 */
	function GetMethodImplementationFlags():cs.system.reflection.MethodImplAttributes;
	/**
	 * Returns the parameters of the dynamic method.
	 * @return An array of  objects that represent the parameters of the dynamic
	 * method.
	 */
	function GetParameters():cs.NativeArray<cs.system.reflection.ParameterInfo>;
	/**
	 * Invokes the dynamic method using the specified parameters, under the constraints
	 * of the specified binder, with the specified culture information.
	 * @param obj This parameter is ignored for dynamic methods, because they are
	 * static. Specify .
	 * @param invokeAttr A bitwise combination of  values.
	 * @param binder A  object that enables the binding, coercion of argument types,
	 * invocation of members, and retrieval of  objects through reflection. If  is ,
	 * the default binder is used. For more details, see .
	 * @param parameters An argument list. This is an array of arguments with the same
	 * number, order, and type as the parameters of the method to be invoked. If there
	 * are no parameters this parameter should be .
	 * @param culture An instance of  used to govern the coercion of types. If this is
	 * , the  for the current thread is used. For example, this information is needed
	 * to correctly convert a  that represents 1000 to a  value, because 1000 is
	 * represented differently by different cultures.
	 * @return A  containing the return value of the invoked method.
	 */
	function Invoke(obj:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, parameters:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic;
	/**
	 * Indicates whether the specified custom attribute type is defined.
	 * @param attributeType A  representing the type of custom attribute to search for.
	 * @param inherit to search the method's inheritance chain to find the custom
	 * attributes;  to check only the current method.
	 * @return if the specified custom attribute type is defined; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Returns the signature of the method, represented as a string.
	 * @return A string representing the method signature.
	 */
	function ToString():String;
}
