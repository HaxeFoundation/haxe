package cs.system.reflection.emit;

/** Defines and creates generic type parameters for dynamically defined generic types and methods. This class cannot be inherited. */
@:native("System.Reflection.Emit.GenericTypeParameterBuilder")
extern class GenericTypeParameterBuilder extends cs.system.Type {
	/**
	 * Tests whether the given object is an instance of  and is equal to the current
	 * instance.
	 * @param o The object to be compared with the current instance.
	 * @return if  is an instance of  and equals the current instance; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetConstructors(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.ConstructorInfo>;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param inherit Specifies whether to search this member's inheritance chain to
	 * find the attributes.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Throws a  in all cases.
	 * @return The type referred to by the current array type, pointer type, or  type;
	 * or  if the current type is not an array type, is not a pointer type, and is not
	 * passed by reference.
	 */
	function GetElementType():cs.system.Type;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param name Not supported.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetEvent(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.EventInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.EventInfo> {})
	/**
	 * Not supported for incomplete generic type parameters.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetEvents(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.EventInfo>;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param name Not supported.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetField(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.FieldInfo;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetFields(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.FieldInfo>;
	/**
	 * Not valid for generic type parameters.
	 * @return Not valid for generic type parameters.
	 */
	function GetGenericArguments():cs.NativeArray<cs.system.Type>;
	/**
	 * Not valid for generic type parameters.
	 * @return Not valid for generic type parameters.
	 */
	function GetGenericTypeDefinition():cs.system.Type;
	/**
	 * Returns a 32-bit integer hash code for the current instance.
	 * @return A 32-bit integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param name The name of the interface.
	 * @param ignoreCase to search without regard for case;  to make a case-sensitive
	 * search.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetInterface(name:String, ignoreCase:Bool):cs.system.Type;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param interfaceType A  object that represents the interface type for which the
	 * mapping is to be retrieved.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetInterfaceMap(interfaceType:cs.system.Type):cs.system.reflection.InterfaceMapping;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetInterfaces():cs.NativeArray<cs.system.Type>;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param name Not supported.
	 * @param type Not supported.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetMember(name:String, type:cs.system.reflection.MemberTypes, bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetMembers(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetMethods(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MethodInfo>;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param name Not supported.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetNestedType(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.Type;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetNestedTypes(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.Type>;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param bindingAttr Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function GetProperties(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.PropertyInfo>;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param name Not supported.
	 * @param invokeAttr Not supported.
	 * @param binder Not supported.
	 * @param target Not supported.
	 * @param args Not supported.
	 * @param modifiers Not supported.
	 * @param culture Not supported.
	 * @param namedParameters Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function InvokeMember(name:String, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, target:Dynamic, args:cs.NativeArray<Dynamic>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>, culture:cs.system.globalization.CultureInfo, namedParameters:cs.NativeArray<String>):Dynamic;
	/**
	 * Throws a  exception in all cases.
	 * @param c The object to test.
	 * @return Throws a  exception in all cases.
	 */
	function IsAssignableFrom(c:cs.system.Type):Bool;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param attributeType Not supported.
	 * @param inherit Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Not supported for incomplete generic type parameters.
	 * @param c Not supported.
	 * @return Not supported for incomplete generic type parameters.
	 */
	function IsSubclassOf(c:cs.system.Type):Bool;
	@:overload(function():cs.system.Type {})
	/**
	 * Returns the type of a one-dimensional array whose element type is the generic
	 * type parameter.
	 * @return A  object that represents the type of a one-dimensional array whose
	 * element type is the generic type parameter.
	 */
	function MakeArrayType(rank:Int):cs.system.Type;
	/**
	 * Returns a  object that represents the current generic type parameter when passed
	 * as a reference parameter.
	 * @return A  object that represents the current generic type parameter when passed
	 * as a reference parameter.
	 */
	function MakeByRefType():cs.system.Type;
	/**
	 * Not valid for incomplete generic type parameters.
	 * @param typeArguments An array of type arguments.
	 * @return This method is invalid for incomplete generic type parameters.
	 */
	function MakeGenericType(typeArguments:cs.NativeArray<cs.system.Type>):cs.system.Type;
	/**
	 * Returns a  object that represents a pointer to the current generic type
	 * parameter.
	 * @return A  object that represents a pointer to the current generic type
	 * parameter.
	 */
	function MakePointerType():cs.system.Type;
	/**
	 * Sets the base type that a type must inherit in order to be substituted for the
	 * type parameter.
	 * @param baseTypeConstraint The  that must be inherited by any type that is to be
	 * substituted for the type parameter.
	 */
	function SetBaseTypeConstraint(baseTypeConstraint:cs.system.Type):Void;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Sets a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attribute.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Sets the variance characteristics and special constraints of the generic
	 * parameter, such as the parameterless constructor constraint.
	 * @param genericParameterAttributes A bitwise combination of  values that
	 * represent the variance characteristics and special constraints of the generic
	 * type parameter.
	 */
	function SetGenericParameterAttributes(genericParameterAttributes:cs.system.reflection.GenericParameterAttributes):Void;
	/**
	 * Sets the interfaces a type must implement in order to be substituted for the
	 * type parameter.
	 * @param interfaceConstraints An array of  objects that represent the interfaces a
	 * type must implement in order to be substituted for the type parameter.
	 */
	function SetInterfaceConstraints(interfaceConstraints:cs.NativeArray<cs.system.Type>):Void;
	/**
	 * Returns a string representation of the current generic type parameter.
	 * @return A string that contains the name of the generic type parameter.
	 */
	function ToString():String;
}
