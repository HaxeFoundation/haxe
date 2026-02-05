package cs.system.reflection;

/** Interoperates with the IDispatch interface. */
@:native("System.Reflection.IReflect")
extern interface IReflect {
	/**
	 * Gets the underlying type that represents the  object.
	 * @return The underlying type that represents the  object.
	 */
	var UnderlyingSystemType(default, never):cs.system.Type;
	/**
	 * Returns the  object that corresponds to the specified field and binding flag.
	 * @param name The name of the field to find.
	 * @param bindingAttr The binding attributes used to control the search.
	 * @return A  object containing the field information for the named object that
	 * meets the search constraints specified in .
	 */
	function GetField(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.FieldInfo;
	/**
	 * Returns an array of  objects that correspond to all fields of the current class.
	 * @param bindingAttr The binding attributes used to control the search.
	 * @return An array of  objects containing all the field information for this
	 * reflection object that meets the search constraints specified in .
	 */
	function GetFields(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.FieldInfo>;
	/**
	 * Retrieves an array of  objects corresponding to all public members or to all
	 * members that match a specified name.
	 * @param name The name of the member to find.
	 * @param bindingAttr The binding attributes used to control the search.
	 * @return An array of  objects matching the  parameter.
	 */
	function GetMember(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Retrieves an array of  objects that correspond either to all public members or
	 * to all members of the current class.
	 * @param bindingAttr The binding attributes used to control the search.
	 * @return An array of  objects containing all the member information for this
	 * reflection object.
	 */
	function GetMembers(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	@:overload(function(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.MethodInfo {})
	/**
	 * Retrieves a  object that corresponds to a specified method under specified
	 * search constraints.
	 * @param name The name of the member to find.
	 * @param bindingAttr The binding attributes used to control the search.
	 * @return A  object containing the method information, with the match being based
	 * on the method name and search constraints specified in .
	 */
	function GetMethod(name:String, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodInfo;
	/**
	 * Retrieves an array of  objects with all public methods or all methods of the
	 * current class.
	 * @param bindingAttr The binding attributes used to control the search.
	 * @return An array of  objects containing all the methods defined for this
	 * reflection object that meet the search constraints specified in .
	 */
	function GetMethods(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MethodInfo>;
	/**
	 * Retrieves an array of  objects corresponding to all public properties or to all
	 * properties of the current class.
	 * @param bindingAttr The binding attribute used to control the search.
	 * @return An array of  objects for all the properties defined on the reflection
	 * object.
	 */
	function GetProperties(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.PropertyInfo>;
	@:overload(function(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.PropertyInfo {})
	/**
	 * Retrieves a  object corresponding to a specified property under specified search
	 * constraints.
	 * @param name The name of the property to find.
	 * @param bindingAttr The binding attributes used to control the search.
	 * @return A  object for the located property that meets the search constraints
	 * specified in , or  if the property was not located.
	 */
	function GetProperty(name:String, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, returnType:cs.system.Type, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.PropertyInfo;
	/**
	 * Invokes a specified member.
	 * @param name The name of the member to find.
	 * @param invokeAttr One of the  invocation attributes. The  parameter may be a
	 * constructor, method, property, or field. A suitable invocation attribute must be
	 * specified. Invoke the default member of a class by passing the empty string ("")
	 * as the name of the member.
	 * @param binder One of the  bit flags. Implements , containing properties related
	 * to this method.
	 * @param target The object on which to invoke the specified member. This parameter
	 * is ignored for static members.
	 * @param args An array of objects that contains the number, order, and type of the
	 * parameters of the member to be invoked. This is an empty array if there are no
	 * parameters.
	 * @param modifiers An array of  objects. This array has the same length as the 
	 * parameter, representing the invoked member's argument attributes in the
	 * metadata. A parameter can have the following attributes: , , , , and . These
	 * represent [In], [Out], [retval], [optional], and a default parameter,
	 * respectively. These attributes are used by various interoperability services.
	 * @param culture An instance of  used to govern the coercion of types. For
	 * example,  converts a  that represents 1000 to a  value, since 1000 is
	 * represented differently by different cultures. If this parameter is , the  for
	 * the current thread is used.
	 * @param namedParameters A  array of parameters.
	 * @return The specified member.
	 */
	function InvokeMember(name:String, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, target:Dynamic, args:cs.NativeArray<Dynamic>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>, culture:cs.system.globalization.CultureInfo, namedParameters:cs.NativeArray<String>):Dynamic;
}
