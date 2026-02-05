package cs.system.reflection.emit;

/** Defines and creates new instances of classes during run time. */
@:native("System.Reflection.Emit.TypeBuilder")
extern class TypeBuilder extends cs.system.Type {
	/** Represents that total size for the type is not specified. */
	static var UnspecifiedTypeSize(default, never):Int;
	/**
	 * Retrieves the packing size of this type.
	 * @return Read-only. Retrieves the packing size of this type.
	 */
	var PackingSize(default, never):cs.system.reflection.emit.PackingSize;
	/**
	 * Retrieves the total size of a type.
	 * @return Read-only. Retrieves this type's total size.
	 */
	var Size(default, never):Int;
	/**
	 * Returns the constructor of the specified constructed generic type that
	 * corresponds to the specified constructor of the generic type definition.
	 * @param type The constructed generic type whose constructor is returned.
	 * @param constructor A constructor on the generic type definition of , which
	 * specifies which constructor of  to return.
	 * @return A  object that represents the constructor of  corresponding to , which
	 * specifies a constructor belonging to the generic type definition of .
	 */
	static function GetConstructor(type:cs.system.Type, constructor:cs.system.reflection.ConstructorInfo):cs.system.reflection.ConstructorInfo;
	/**
	 * Returns the field specified by the given name.
	 * @param name The name of the field to get.
	 * @param bindingAttr This must be a bit flag from  as in , , and so on.
	 * @return Returns the  object representing the field declared or inherited by this
	 * type with the specified name and public or non-public modifier. If there are no
	 * matches then  is returned.
	 */
	static function GetField(type:cs.system.Type, field:cs.system.reflection.FieldInfo):cs.system.reflection.FieldInfo;
	/**
	 * Returns the method of the specified constructed generic type that corresponds to
	 * the specified method of the generic type definition.
	 * @param type The constructed generic type whose method is returned.
	 * @param method A method on the generic type definition of , which specifies which
	 * method of  to return.
	 * @return A  object that represents the method of  corresponding to , which
	 * specifies a method belonging to the generic type definition of .
	 */
	static function GetMethod(type:cs.system.Type, method:cs.system.reflection.MethodInfo):cs.system.reflection.MethodInfo;
	/**
	 * Adds an interface that this type implements.
	 * @param interfaceType The interface that this type implements.
	 */
	function AddInterfaceImplementation(interfaceType:cs.system.Type):Void;
	/**
	 * Creates a  object for the class. After defining fields and methods on the class,
	 * is called in order to load its  object.
	 * @return Returns the new  object for this class.
	 */
	function CreateType():cs.system.Type;
	/**
	 * Gets a  object that represents this type.
	 * @return An object that represents this type.
	 */
	function CreateTypeInfo():cs.system.reflection.TypeInfo;
	@:overload(function(attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.ConstructorBuilder {})
	/**
	 * Adds a new constructor to the type, with the given attributes and signature.
	 * @param attributes The attributes of the constructor.
	 * @param callingConvention The calling convention of the constructor.
	 * @param parameterTypes The parameter types of the constructor.
	 * @return The defined constructor.
	 */
	function DefineConstructor(attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, parameterTypes:cs.NativeArray<cs.system.Type>, requiredCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, optionalCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):cs.system.reflection.emit.ConstructorBuilder;
	/**
	 * Defines the parameterless constructor. The constructor defined here will simply
	 * call the parameterless constructor of the parent.
	 * @param attributes A  object representing the attributes to be applied to the
	 * constructor.
	 * @return Returns the constructor.
	 */
	function DefineDefaultConstructor(attributes:cs.system.reflection.MethodAttributes):cs.system.reflection.emit.ConstructorBuilder;
	/**
	 * Adds a new event to the type, with the given name, attributes and event type.
	 * @param name The name of the event.  cannot contain embedded nulls.
	 * @param attributes The attributes of the event.
	 * @param eventtype The type of the event.
	 * @return The defined event.
	 */
	function DefineEvent(name:String, attributes:cs.system.reflection.EventAttributes, eventtype:cs.system.Type):cs.system.reflection.emit.EventBuilder;
	@:overload(function(fieldName:String, type:cs.system.Type, attributes:cs.system.reflection.FieldAttributes):cs.system.reflection.emit.FieldBuilder {})
	/**
	 * Adds a new field to the type, with the given name, attributes, and field type.
	 * @param fieldName The name of the field.  cannot contain embedded nulls.
	 * @param type The type of the field
	 * @param attributes The attributes of the field.
	 * @return The defined field.
	 */
	function DefineField(fieldName:String, type:cs.system.Type, requiredCustomModifiers:cs.NativeArray<cs.system.Type>, optionalCustomModifiers:cs.NativeArray<cs.system.Type>, attributes:cs.system.reflection.FieldAttributes):cs.system.reflection.emit.FieldBuilder;
	/**
	 * Defines the generic type parameters for the current type, specifying their
	 * number and their names, and returns an array of  objects that can be used to set
	 * their constraints.
	 * @param names An array of names for the generic type parameters.
	 * @return An array of  objects that can be used to define the constraints of the
	 * generic type parameters for the current type.
	 */
	function DefineGenericParameters(names:cs.NativeArray<String>):cs.NativeArray<cs.system.reflection.emit.GenericTypeParameterBuilder>;
	/**
	 * Defines initialized data field in the .sdata section of the portable executable
	 * (PE) file.
	 * @param name The name used to refer to the data.  cannot contain embedded nulls.
	 * @param data The blob of data.
	 * @param attributes The attributes for the field.
	 * @return A field to reference the data.
	 */
	function DefineInitializedData(name:String, data:cs.NativeArray<cs.UInt8>, attributes:cs.system.reflection.FieldAttributes):cs.system.reflection.emit.FieldBuilder;
	@:overload(function(name:String, attributes:cs.system.reflection.MethodAttributes):cs.system.reflection.emit.MethodBuilder {})
	@:overload(function(name:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions):cs.system.reflection.emit.MethodBuilder {})
	@:overload(function(name:String, attributes:cs.system.reflection.MethodAttributes, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.MethodBuilder {})
	@:overload(function(name:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.MethodBuilder {})
	/**
	 * Adds a new method to the type, with the specified name and method attributes.
	 * @param name The name of the method.  cannot contain embedded nulls.
	 * @param attributes The attributes of the method.
	 * @return A  representing the newly defined method.
	 */
	function DefineMethod(name:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, returnTypeRequiredCustomModifiers:cs.NativeArray<cs.system.Type>, returnTypeOptionalCustomModifiers:cs.NativeArray<cs.system.Type>, parameterTypes:cs.NativeArray<cs.system.Type>, parameterTypeRequiredCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, parameterTypeOptionalCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):cs.system.reflection.emit.MethodBuilder;
	/**
	 * Specifies a given method body that implements a given method declaration,
	 * potentially with a different name.
	 * @param methodInfoBody The method body to be used. This should be a  object.
	 * @param methodInfoDeclaration The method whose declaration is to be used.
	 */
	function DefineMethodOverride(methodInfoBody:cs.system.reflection.MethodInfo, methodInfoDeclaration:cs.system.reflection.MethodInfo):Void;
	@:overload(function(name:String):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type, typeSize:Int):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type, packSize:cs.system.reflection.emit.PackingSize):cs.system.reflection.emit.TypeBuilder {})
	@:overload(function(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type, interfaces:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.TypeBuilder {})
	/**
	 * Defines a nested type, given its name.
	 * @param name The short name of the type.  cannot contain embedded nulls.
	 * @return The defined nested type.
	 */
	function DefineNestedType(name:String, attr:cs.system.reflection.TypeAttributes, parent:cs.system.Type, packSize:cs.system.reflection.emit.PackingSize, typeSize:Int):cs.system.reflection.emit.TypeBuilder;
	@:overload(function(name:String, dllName:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, nativeCallConv:cs.system.runtime.interopservices.CallingConvention, nativeCharSet:cs.system.runtime.interopservices.CharSet):cs.system.reflection.emit.MethodBuilder {})
	@:overload(function(name:String, dllName:String, entryName:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, nativeCallConv:cs.system.runtime.interopservices.CallingConvention, nativeCharSet:cs.system.runtime.interopservices.CharSet):cs.system.reflection.emit.MethodBuilder {})
	/**
	 * Defines a  method given its name, the name of the DLL in which the method is
	 * defined, the attributes of the method, the calling convention of the method, the
	 * return type of the method, the types of the parameters of the method, and the 
	 * flags.
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
	function DefinePInvokeMethod(name:String, dllName:String, entryName:String, attributes:cs.system.reflection.MethodAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, returnTypeRequiredCustomModifiers:cs.NativeArray<cs.system.Type>, returnTypeOptionalCustomModifiers:cs.NativeArray<cs.system.Type>, parameterTypes:cs.NativeArray<cs.system.Type>, parameterTypeRequiredCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, parameterTypeOptionalCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, nativeCallConv:cs.system.runtime.interopservices.CallingConvention, nativeCharSet:cs.system.runtime.interopservices.CharSet):cs.system.reflection.emit.MethodBuilder;
	@:overload(function(name:String, attributes:cs.system.reflection.PropertyAttributes, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.PropertyBuilder {})
	@:overload(function(name:String, attributes:cs.system.reflection.PropertyAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.PropertyBuilder {})
	@:overload(function(name:String, attributes:cs.system.reflection.PropertyAttributes, returnType:cs.system.Type, returnTypeRequiredCustomModifiers:cs.NativeArray<cs.system.Type>, returnTypeOptionalCustomModifiers:cs.NativeArray<cs.system.Type>, parameterTypes:cs.NativeArray<cs.system.Type>, parameterTypeRequiredCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, parameterTypeOptionalCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):cs.system.reflection.emit.PropertyBuilder {})
	/**
	 * Adds a new property to the type, with the given name, attributes, calling
	 * convention, and property signature.
	 * @param name The name of the property.  cannot contain embedded nulls.
	 * @param attributes The attributes of the property.
	 * @param callingConvention The calling convention of the property accessors.
	 * @param returnType The return type of the property.
	 * @param parameterTypes The types of the parameters of the property.
	 * @return The defined property.
	 */
	function DefineProperty(name:String, attributes:cs.system.reflection.PropertyAttributes, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, returnTypeRequiredCustomModifiers:cs.NativeArray<cs.system.Type>, returnTypeOptionalCustomModifiers:cs.NativeArray<cs.system.Type>, parameterTypes:cs.NativeArray<cs.system.Type>, parameterTypeRequiredCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, parameterTypeOptionalCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):cs.system.reflection.emit.PropertyBuilder;
	/**
	 * Defines the initializer for this type.
	 * @return Returns a type initializer.
	 */
	function DefineTypeInitializer():cs.system.reflection.emit.ConstructorBuilder;
	/**
	 * Defines an uninitialized data field in the  section of the portable executable
	 * (PE) file.
	 * @param name The name used to refer to the data.  cannot contain embedded nulls.
	 * @param size The size of the data field.
	 * @param attributes The attributes for the field.
	 * @return A field to reference the data.
	 */
	function DefineUninitializedData(name:String, size:Int, attributes:cs.system.reflection.FieldAttributes):cs.system.reflection.emit.FieldBuilder;
	/**
	 * Returns an array of  objects representing the public and non-public constructors
	 * defined for this class, as specified.
	 * @param bindingAttr This must be a bit flag from  as in , , and so on.
	 * @return Returns an array of  objects representing the specified constructors
	 * defined for this class. If no constructors are defined, an empty array is
	 * returned.
	 */
	function GetConstructors(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.ConstructorInfo>;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns all the custom attributes defined for this type.
	 * @param inherit Specifies whether to search this member's inheritance chain to
	 * find the attributes.
	 * @return Returns an array of objects representing all the custom attributes of
	 * this type.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Calling this method always throws .
	 * @return This method is not supported. No value is returned.
	 */
	function GetElementType():cs.system.Type;
	/**
	 * Returns the event with the specified name.
	 * @param name The name of the event to search for.
	 * @param bindingAttr A bitwise combination of  values that limits the search.
	 * @return An  object representing the event declared or inherited by this type
	 * with the specified name, or  if there are no matches.
	 */
	function GetEvent(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.EventInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.EventInfo> {})
	/**
	 * Returns the public events declared or inherited by this type.
	 * @return Returns an array of  objects representing the public events declared or
	 * inherited by this type. An empty array is returned if there are no public
	 * events.
	 */
	function GetEvents(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.EventInfo>;
	/**
	 * Returns the field specified by the given name.
	 * @param name The name of the field to get.
	 * @param bindingAttr This must be a bit flag from  as in , , and so on.
	 * @return Returns the  object representing the field declared or inherited by this
	 * type with the specified name and public or non-public modifier. If there are no
	 * matches then  is returned.
	 */
	function GetField(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.FieldInfo;
	/**
	 * Returns the public and non-public fields that are declared by this type.
	 * @param bindingAttr This must be a bit flag from  : , , and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * fields declared or inherited by this type. An empty array is returned if there
	 * are no fields, as specified.
	 */
	function GetFields(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.FieldInfo>;
	/**
	 * Returns an array of  objects representing the type arguments of a generic type
	 * or the type parameters of a generic type definition.
	 * @return An array of  objects. The elements of the array represent the type
	 * arguments of a generic type or the type parameters of a generic type definition.
	 */
	function GetGenericArguments():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns a  object that represents a generic type definition from which the
	 * current type can be obtained.
	 * @return A  object representing a generic type definition from which the current
	 * type can be obtained.
	 */
	function GetGenericTypeDefinition():cs.system.Type;
	/**
	 * Returns the interface implemented (directly or indirectly) by this class with
	 * the fully qualified name matching the given interface name.
	 * @param name The name of the interface.
	 * @param ignoreCase If , the search is case-insensitive. If , the search is
	 * case-sensitive.
	 * @return Returns a  object representing the implemented interface. Returns null
	 * if no interface matching name is found.
	 */
	function GetInterface(name:String, ignoreCase:Bool):cs.system.Type;
	/**
	 * Returns an interface mapping for the requested interface.
	 * @param interfaceType The  of the interface for which the mapping is to be
	 * retrieved.
	 * @return Returns the requested interface mapping.
	 */
	function GetInterfaceMap(interfaceType:cs.system.Type):cs.system.reflection.InterfaceMapping;
	/**
	 * Returns an array of all the interfaces implemented on this type and its base
	 * types.
	 * @return Returns an array of  objects representing the implemented interfaces. If
	 * none are defined, an empty array is returned.
	 */
	function GetInterfaces():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns all the public and non-public members declared or inherited by this
	 * type, as specified.
	 * @param name The name of the member.
	 * @param type The type of the member to return.
	 * @param bindingAttr This must be a bit flag from , as in , , and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * members defined on this type if  is used; otherwise, only the public members are
	 * returned.
	 */
	function GetMember(name:String, type:cs.system.reflection.MemberTypes, bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Returns the members for the public and non-public members declared or inherited
	 * by this type.
	 * @param bindingAttr This must be a bit flag from , such as , , and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * members declared or inherited by this type. An empty array is returned if there
	 * are no matching members.
	 */
	function GetMembers(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Returns all the public and non-public methods declared or inherited by this
	 * type, as specified.
	 * @param bindingAttr This must be a bit flag from  as in , , and so on.
	 * @return Returns an array of  objects representing the public and non-public
	 * methods defined on this type if  is used; otherwise, only the public methods are
	 * returned.
	 */
	function GetMethods(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MethodInfo>;
	/**
	 * Returns the public and non-public nested types that are declared by this type.
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
	 * @param bindingAttr This must be a bit flag from , as in , , and so on.
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
	 * @param args An argument list. This is an array of Objects that contains the
	 * number, order, and type of the parameters of the member to be invoked. If there
	 * are no parameters this should be null.
	 * @param modifiers An array of the same length as  with elements that represent
	 * the attributes associated with the arguments of the member to be invoked. A
	 * parameter has attributes associated with it in the metadata. They are used by
	 * various interoperability services. See the metadata specs for more details.
	 * @param culture An instance of  used to govern the coercion of types. If this is
	 * null, the  for the current thread is used. (Note that this is necessary to, for
	 * example, convert a String that represents 1000 to a Double value, since 1000 is
	 * represented differently by different cultures.)
	 * @param namedParameters Each parameter in the  array gets the value in the
	 * corresponding element in the  array. If the length of  is greater than the
	 * length of , the remaining argument values are passed in order.
	 * @return Returns the return value of the invoked member.
	 */
	function InvokeMember(name:String, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, target:Dynamic, args:cs.NativeArray<Dynamic>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>, culture:cs.system.globalization.CultureInfo, namedParameters:cs.NativeArray<String>):Dynamic;
	/**
	 * Gets a value that indicates whether a specified  can be assigned to this object.
	 * @param c The object to test.
	 * @return if the  parameter and the current type represent the same type, or if
	 * the current type is in the inheritance hierarchy of , or if the current type is
	 * an interface that  supports.  if none of these conditions are valid, or if  is .
	 */
	function IsAssignableFrom(c:cs.system.Type):Bool;
	/**
	 * Returns a value that indicates whether the current dynamic type has been
	 * created.
	 * @return if the  method has been called; otherwise, .
	 */
	function IsCreated():Bool;
	/**
	 * Determines whether a custom attribute is applied to the current type.
	 * @param attributeType The type of attribute to search for. Only attributes that
	 * are assignable to this type are returned.
	 * @param inherit Specifies whether to search this member's inheritance chain to
	 * find the attributes.
	 * @return if one or more instances of , or an attribute derived from , is defined
	 * on this type; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Determines whether this type is derived from a specified type.
	 * @param c A  that is to be checked.
	 * @return Read-only. Returns  if this type is the same as the type , or is a
	 * subtype of type ; otherwise, .
	 */
	function IsSubclassOf(c:cs.system.Type):Bool;
	@:overload(function():cs.system.Type {})
	/**
	 * Returns a  object that represents a one-dimensional array of the current type,
	 * with a lower bound of zero.
	 * @return A  object representing a one-dimensional array type whose element type
	 * is the current type, with a lower bound of zero.
	 */
	function MakeArrayType(rank:Int):cs.system.Type;
	/**
	 * Returns a  object that represents the current type when passed as a  parameter (
	 * in Visual Basic).
	 * @return A  object that represents the current type when passed as a  parameter (
	 * in Visual Basic).
	 */
	function MakeByRefType():cs.system.Type;
	/**
	 * Substitutes the elements of an array of types for the type parameters of the
	 * current generic type definition, and returns the resulting constructed type.
	 * @param typeArguments An array of types to be substituted for the type parameters
	 * of the current generic type definition.
	 * @return A  representing the constructed type formed by substituting the elements
	 * of  for the type parameters of the current generic type.
	 */
	function MakeGenericType(typeArguments:cs.NativeArray<cs.system.Type>):cs.system.Type;
	/**
	 * Returns a  object that represents the type of an unmanaged pointer to the
	 * current type.
	 * @return A  object that represents the type of an unmanaged pointer to the
	 * current type.
	 */
	function MakePointerType():cs.system.Type;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Sets a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Sets the base type of the type currently under construction.
	 * @param parent The new base type.
	 */
	function SetParent(parent:cs.system.Type):Void;
	/**
	 * Returns the name of the type excluding the namespace.
	 * @return Read-only. The name of the type excluding the namespace.
	 */
	function ToString():String;
}
