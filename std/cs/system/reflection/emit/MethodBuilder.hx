package cs.system.reflection.emit;

/** Defines and represents a method (or constructor) on a dynamic class. */
@:native("System.Reflection.Emit.MethodBuilder")
extern class MethodBuilder extends cs.system.reflection.MethodInfo {
	/**
	 * Gets or sets a Boolean value that specifies whether the local variables in this
	 * method are zero initialized. The default value of this property is .
	 * @return if the local variables in this method should be zero initialized;
	 * otherwise .
	 */
	var InitLocals(default, default):Bool;
	/**
	 * Sets the number of generic type parameters for the current method, specifies
	 * their names, and returns an array of  objects that can be used to define their
	 * constraints.
	 * @param names An array of strings that represent the names of the generic type
	 * parameters.
	 * @return An array of  objects representing the type parameters of the generic
	 * method.
	 */
	function DefineGenericParameters(names:cs.NativeArray<String>):cs.NativeArray<cs.system.reflection.emit.GenericTypeParameterBuilder>;
	/**
	 * Sets the parameter attributes and the name of a parameter of this method, or of
	 * the return value of this method. Returns a ParameterBuilder that can be used to
	 * apply custom attributes.
	 * @param position The position of the parameter in the parameter list. Parameters
	 * are indexed beginning with the number 1 for the first parameter; the number 0
	 * represents the return value of the method.
	 * @param attributes The parameter attributes of the parameter.
	 * @param strParamName The name of the parameter. The name can be the null string.
	 * @return Returns a  object that represents a parameter of this method or the
	 * return value of this method.
	 */
	function DefineParameter(position:Int, attributes:cs.system.reflection.ParameterAttributes, strParamName:String):cs.system.reflection.emit.ParameterBuilder;
	/**
	 * Determines whether the given object is equal to this instance.
	 * @param obj The object to compare with this  instance.
	 * @return if  is an instance of  and is equal to this object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Return the base implementation for a method.
	 * @return The base implementation of this method.
	 */
	function GetBaseDefinition():cs.system.reflection.MethodInfo;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns all the custom attributes defined for this method.
	 * @param inherit Specifies whether to search this member's inheritance chain to
	 * find the custom attributes.
	 * @return Returns an array of objects representing all the custom attributes of
	 * this method.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Returns an array of  objects that represent the type parameters of the method,
	 * if it is generic.
	 * @return An array of  objects representing the type parameters, if the method is
	 * generic, or  if the method is not generic.
	 */
	function GetGenericArguments():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns this method.
	 * @return The current instance of .
	 */
	function GetGenericMethodDefinition():cs.system.reflection.MethodInfo;
	/**
	 * Gets the hash code for this method.
	 * @return The hash code for this method.
	 */
	function GetHashCode():Int;
	@:overload(function():cs.system.reflection.emit.ILGenerator {})
	/**
	 * Returns an  for this method with a default Microsoft intermediate language
	 * (MSIL) stream size of 64 bytes.
	 * @return Returns an  object for this method.
	 */
	function GetILGenerator(size:Int):cs.system.reflection.emit.ILGenerator;
	/**
	 * Returns the implementation flags for the method.
	 * @return Returns the implementation flags for the method.
	 */
	function GetMethodImplementationFlags():cs.system.reflection.MethodImplAttributes;
	/**
	 * Returns the parameters of this method.
	 * @return An array of  objects that represent the parameters of the method.
	 */
	function GetParameters():cs.NativeArray<cs.system.reflection.ParameterInfo>;
	/**
	 * Dynamically invokes the method reflected by this instance on the given object,
	 * passing along the specified parameters, and under the constraints of the given
	 * binder.
	 * @param obj The object on which to invoke the specified method. If the method is
	 * static, this parameter is ignored.
	 * @param invokeAttr This must be a bit flag from  : , , and so on.
	 * @param binder An object that enables the binding, coercion of argument types,
	 * invocation of members, and retrieval of MemberInfo objects via reflection. If
	 * binder is , the default binder is used. For more details, see .
	 * @param parameters An argument list. This is an array of arguments with the same
	 * number, order, and type as the parameters of the method to be invoked. If there
	 * are no parameters this should be .
	 * @param culture An instance of  used to govern the coercion of types. If this is
	 * null, the  for the current thread is used. (Note that this is necessary to, for
	 * example, convert a  that represents 1000 to a  value, since 1000 is represented
	 * differently by different cultures.)
	 * @return Returns an object containing the return value of the invoked method.
	 */
	function Invoke(obj:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, parameters:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic;
	/**
	 * Checks if the specified custom attribute type is defined.
	 * @param attributeType The custom attribute type.
	 * @param inherit Specifies whether to search this member's inheritance chain to
	 * find the custom attributes.
	 * @return if the specified custom attribute type is defined; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Returns a generic method constructed from the current generic method definition
	 * using the specified generic type arguments.
	 * @param typeArguments An array of  objects that represent the type arguments for
	 * the generic method.
	 * @return A  representing the generic method constructed from the current generic
	 * method definition using the specified generic type arguments.
	 */
	function MakeGenericMethod(typeArguments:cs.NativeArray<cs.system.Type>):cs.system.reflection.MethodInfo;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Sets a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Sets the implementation flags for this method.
	 * @param attributes The implementation flags to set.
	 */
	function SetImplementationFlags(attributes:cs.system.reflection.MethodImplAttributes):Void;
	/**
	 * Sets the number and types of parameters for a method.
	 * @param parameterTypes An array of  objects representing the parameter types.
	 */
	function SetParameters(parameterTypes:cs.NativeArray<cs.system.Type>):Void;
	/**
	 * Sets the return type of the method.
	 * @param returnType A  object that represents the return type of the method.
	 */
	function SetReturnType(returnType:cs.system.Type):Void;
	/**
	 * Sets the method signature, including the return type, the parameter types, and
	 * the required and optional custom modifiers of the return type and parameter
	 * types.
	 * @param returnType The return type of the method.
	 * @param returnTypeRequiredCustomModifiers An array of types representing the
	 * required custom modifiers, such as , for the return type of the method. If the
	 * return type has no required custom modifiers, specify .
	 * @param returnTypeOptionalCustomModifiers An array of types representing the
	 * optional custom modifiers, such as , for the return type of the method. If the
	 * return type has no optional custom modifiers, specify .
	 * @param parameterTypes The types of the parameters of the method.
	 * @param parameterTypeRequiredCustomModifiers An array of arrays of types. Each
	 * array of types represents the required custom modifiers for the corresponding
	 * parameter, such as . If a particular parameter has no required custom modifiers,
	 * specify  instead of an array of types. If none of the parameters have required
	 * custom modifiers, specify  instead of an array of arrays.
	 * @param parameterTypeOptionalCustomModifiers An array of arrays of types. Each
	 * array of types represents the optional custom modifiers for the corresponding
	 * parameter, such as . If a particular parameter has no optional custom modifiers,
	 * specify  instead of an array of types. If none of the parameters have optional
	 * custom modifiers, specify  instead of an array of arrays.
	 */
	function SetSignature(returnType:cs.system.Type, returnTypeRequiredCustomModifiers:cs.NativeArray<cs.system.Type>, returnTypeOptionalCustomModifiers:cs.NativeArray<cs.system.Type>, parameterTypes:cs.NativeArray<cs.system.Type>, parameterTypeRequiredCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, parameterTypeOptionalCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):Void;
	/**
	 * Returns this  instance as a string.
	 * @return Returns a string containing the name, attributes, method signature,
	 * exceptions, and local signature of this method followed by the current Microsoft
	 * intermediate language (MSIL) stream.
	 */
	function ToString():String;
}
