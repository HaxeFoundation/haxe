package cs.system.reflection.emit;

/** Describes and represents an enumeration type. */
@:native("System.Reflection.Emit.EnumBuilder")
extern class EnumBuilder extends cs.system.Type {
	/**
	 * Returns the underlying field for this enum.
	 * @return Read-only. The underlying field for this enum.
	 */
	var UnderlyingField(default, never):cs.system.reflection.emit.FieldBuilder;
	/**
	 * Gets a  object that represents this enumeration.
	 * @return An object that represents this enumeration.
	 */
	function CreateTypeInfo():cs.system.reflection.TypeInfo;
	/**
	 * Defines the named static field in an enumeration type with the specified
	 * constant value.
	 * @param literalName The name of the static field.
	 * @param literalValue The constant value of the literal.
	 * @return The defined field.
	 */
	function DefineLiteral(literalName:String, literalValue:Dynamic):cs.system.reflection.emit.FieldBuilder;
	/**
	 * Returns an array of  objects representing the public and non-public constructors
	 * defined for this class, as specified.
	 * @param bindingAttr This must be a bit flag from  : , , and so on.
	 * @return Returns an array of  objects representing the specified constructors
	 * defined for this class. If no constructors are defined, an empty array is
	 * returned.
	 */
	function GetConstructors(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.ConstructorInfo>;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns all the custom attributes defined for this constructor.
	 * @param inherit Specifies whether to search this member's inheritance chain to
	 * find the attributes.
	 * @return Returns an array of objects representing all the custom attributes of
	 * the constructor represented by this  instance.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Calling this method always throws .
	 * @return This method is not supported. No value is returned.
	 */
	function GetElementType():cs.system.Type;
	/**
	 * Returns the underlying integer type of the current enumeration, which is set
	 * when the enumeration builder is defined.
	 * @return The underlying type.
	 */
	function GetEnumUnderlyingType():cs.system.Type;
	/**
	 * Returns the event with the specified name.
	 * @param name The name of the event to get.
	 * @param bindingAttr This invocation attribute. This must be a bit flag from  : ,
	 * , and so on.
	 * @return Returns an  object representing the event declared or inherited by this
	 * type with the specified name. If there are no matches,  is returned.
	 */
	function GetEvent(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.EventInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.EventInfo> {})
	/**
	 * Returns the events for the public events declared or inherited by this type.
	 * @return Returns an array of  objects representing the public events declared or
	 * inherited by this type. An empty array is returned if there are no public
	 * events.
	 */
	function GetEvents(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.EventInfo>;
	/**
	 * Returns the field specified by the given name.
	 * @param name The name of the field to get.
	 * @param bindingAttr This must be a bit flag from  : , , and so on.
	 * @return Returns the  object representing the field declared or inherited by this
	 * type with the specified name and public or non-public modifier. If there are no
	 * matches, then null is returned.
	 */
	function GetField(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.FieldInfo;
	/**
	 * Returns the public and non-public fields that are declared by this type.
	 * @param bindingAttr This must be a bit flag from , such as InvokeMethod,
	 * NonPublic, and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * fields declared or inherited by this type. An empty array is returned if there
	 * are no fields, as specified.
	 */
	function GetFields(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.FieldInfo>;
	/**
	 * Returns the interface implemented (directly or indirectly) by this type, with
	 * the specified fully-qualified name.
	 * @param name The name of the interface.
	 * @param ignoreCase If , the search is case-insensitive. If , the search is
	 * case-sensitive.
	 * @return Returns a  object representing the implemented interface. Returns null
	 * if no interface matching name is found.
	 */
	function GetInterface(name:String, ignoreCase:Bool):cs.system.Type;
	/**
	 * Returns an interface mapping for the interface requested.
	 * @param interfaceType The type of the interface for which the interface mapping
	 * is to be retrieved.
	 * @return The requested interface mapping.
	 */
	function GetInterfaceMap(interfaceType:cs.system.Type):cs.system.reflection.InterfaceMapping;
	/**
	 * Returns an array of all the interfaces implemented on this a class and its base
	 * classes.
	 * @return Returns an array of  objects representing the implemented interfaces. If
	 * none are defined, an empty array is returned.
	 */
	function GetInterfaces():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns all members with the specified name, type, and binding that are declared
	 * or inherited by this type.
	 * @param name The name of the member.
	 * @param type The type of member that is to be returned.
	 * @param bindingAttr This must be a bit flag from  : , , and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * members defined on this type if  is used; otherwise, only the public members are
	 * returned.
	 */
	function GetMember(name:String, type:cs.system.reflection.MemberTypes, bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Returns the specified members declared or inherited by this type,.
	 * @param bindingAttr This must be a bit flag from  : , , and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * members declared or inherited by this type. An empty array is returned if there
	 * are no matching members.
	 */
	function GetMembers(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Returns all the public and non-public methods declared or inherited by this
	 * type, as specified.
	 * @param bindingAttr This must be a bit flag from , such as , , and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * methods defined on this type if  is used; otherwise, only the public methods are
	 * returned.
	 */
	function GetMethods(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MethodInfo>;
	/**
	 * Returns the specified nested type that is declared by this type.
	 * @param name The  containing the name of the nested type to get.
	 * @param bindingAttr A bitmask comprised of one or more  that specify how the
	 * search is conducted. -or- Zero, to conduct a case-sensitive search for public
	 * methods.
	 * @return A  object representing the nested type that matches the specified
	 * requirements, if found; otherwise, .
	 */
	function GetNestedType(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.Type;
	/**
	 * Returns the public and non-public nested types that are declared or inherited by
	 * this type.
	 * @param bindingAttr This must be a bit flag from , such as , , and so on.
	 * @return An array of  objects representing all the types nested within the
	 * current  that match the specified binding constraints. An empty array of type ,
	 * if no types are nested within the current , or if none of the nested types match
	 * the binding constraints.
	 */
	function GetNestedTypes(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.Type>;
	/**
	 * Returns all the public and non-public properties declared or inherited by this
	 * type, as specified.
	 * @param bindingAttr This invocation attribute. This must be a bit flag from  : ,
	 * , and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * properties defined on this type if  is used; otherwise, only the public
	 * properties are returned.
	 */
	function GetProperties(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.PropertyInfo>;
	/**
	 * Invokes the specified member. The method that is to be invoked must be
	 * accessible and provide the most specific match with the specified argument list,
	 * under the constraints of the specified binder and invocation attributes.
	 * @param name The name of the member to invoke. This can be a constructor, method,
	 * property, or field. A suitable invocation attribute must be specified. Note that
	 * it is possible to invoke the default member of a class by passing an empty
	 * string as the name of the member.
	 * @param invokeAttr The invocation attribute. This must be a bit flag from .
	 * @param binder An object that enables the binding, coercion of argument types,
	 * invocation of members, and retrieval of  objects using reflection. If binder is
	 * , the default binder is used. See .
	 * @param target The object on which to invoke the specified member. If the member
	 * is static, this parameter is ignored.
	 * @param args An argument list. This is an array of objects that contains the
	 * number, order, and type of the parameters of the member to be invoked. If there
	 * are no parameters this should be null.
	 * @param modifiers An array of the same length as  with elements that represent
	 * the attributes associated with the arguments of the member to be invoked. A
	 * parameter has attributes associated with it in the metadata. They are used by
	 * various interoperability services. See the metadata specs for details such as
	 * this.
	 * @param culture An instance of  used to govern the coercion of types. If this is
	 * null, the  for the current thread is used. (Note that this is necessary to, for
	 * example, convert a string that represents 1000 to a double value, since 1000 is
	 * represented differently by different cultures.)
	 * @param namedParameters Each parameter in the  array gets the value in the
	 * corresponding element in the  array. If the length of  is greater than the
	 * length of , the remaining argument values are passed in order.
	 * @return Returns the return value of the invoked member.
	 */
	function InvokeMember(name:String, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, target:Dynamic, args:cs.NativeArray<Dynamic>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>, culture:cs.system.globalization.CultureInfo, namedParameters:cs.NativeArray<String>):Dynamic;
	/**
	 * Checks if the specified custom attribute type is defined.
	 * @param attributeType The  object to which the custom attributes are applied.
	 * @param inherit Specifies whether to search this member's inheritance chain to
	 * find the attributes.
	 * @return if one or more instance of  is defined on this member; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	@:overload(function():cs.system.Type {})
	/**
	 * Returns a  object representing a one-dimensional array of the current type, with
	 * a lower bound of zero.
	 * @return A  object representing a one-dimensional array of the current type, with
	 * a lower bound of zero.
	 */
	function MakeArrayType(rank:Int):cs.system.Type;
	/**
	 * Returns a  object that represents the current type when passed as a ref
	 * parameter (ByRef parameter in Visual Basic).
	 * @return A  object that represents the current type when passed as a ref
	 * parameter (ByRef parameter in Visual Basic).
	 */
	function MakeByRefType():cs.system.Type;
	/**
	 * Returns a  object that represents a pointer to the current type.
	 * @return A  object that represents a pointer to the current type.
	 */
	function MakePointerType():cs.system.Type;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Sets a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
}
