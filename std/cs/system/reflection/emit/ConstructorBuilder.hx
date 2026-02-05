package cs.system.reflection.emit;

/** Defines and represents a constructor of a dynamic class. */
@:native("System.Reflection.Emit.ConstructorBuilder")
extern class ConstructorBuilder extends cs.system.reflection.ConstructorInfo {
	/**
	 * Gets or sets whether the local variables in this constructor should be
	 * zero-initialized.
	 * @return Read/write. Gets or sets whether the local variables in this constructor
	 * should be zero-initialized.
	 */
	var InitLocals(default, default):Bool;
	/**
	 * Defines a parameter of this constructor.
	 * @param iSequence The position of the parameter in the parameter list. Parameters
	 * are indexed beginning with the number 1 for the first parameter.
	 * @param attributes The attributes of the parameter.
	 * @param strParamName The name of the parameter. The name can be the null string.
	 * @return An object that represents the new parameter of this constructor.
	 */
	function DefineParameter(iSequence:Int, attributes:cs.system.reflection.ParameterAttributes, strParamName:String):cs.system.reflection.emit.ParameterBuilder;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns all the custom attributes defined for this constructor.
	 * @param inherit Controls inheritance of custom attributes from base classes. This
	 * parameter is ignored.
	 * @return An array of objects representing all the custom attributes of the
	 * constructor represented by this  instance.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	@:overload(function():cs.system.reflection.emit.ILGenerator {})
	/**
	 * Gets an  for this constructor.
	 * @return An  object for this constructor.
	 */
	function GetILGenerator(streamSize:Int):cs.system.reflection.emit.ILGenerator;
	/**
	 * Returns the method implementation flags for this constructor.
	 * @return The method implementation flags for this constructor.
	 */
	function GetMethodImplementationFlags():cs.system.reflection.MethodImplAttributes;
	/**
	 * Returns the parameters of this constructor.
	 * @return An array that represents the parameters of this constructor.
	 */
	function GetParameters():cs.NativeArray<cs.system.reflection.ParameterInfo>;
	@:overload(function(invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, parameters:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic {})
	/**
	 * Dynamically invokes the constructor reflected by this instance with the
	 * specified arguments, under the constraints of the specified .
	 * @param obj The object that needs to be reinitialized.
	 * @param invokeAttr One of the  values that specifies the type of binding that is
	 * desired.
	 * @param binder A  that defines a set of properties and enables the binding,
	 * coercion of argument types, and invocation of members using reflection. If  is ,
	 * then Binder.DefaultBinding is used.
	 * @param parameters An argument list. This is an array of arguments with the same
	 * number, order, and type as the parameters of the constructor to be invoked. If
	 * there are no parameters, this should be a null reference ( in Visual Basic).
	 * @param culture A  used to govern the coercion of types. If this is null, the 
	 * for the current thread is used.
	 * @return An instance of the class associated with the constructor.
	 */
	function Invoke(obj:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, parameters:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic;
	/**
	 * Checks if the specified custom attribute type is defined.
	 * @param attributeType A custom attribute type.
	 * @param inherit Controls inheritance of custom attributes from base classes. This
	 * parameter is ignored.
	 * @return if the specified custom attribute type is defined; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Set a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Sets the method implementation flags for this constructor.
	 * @param attributes The method implementation flags.
	 */
	function SetImplementationFlags(attributes:cs.system.reflection.MethodImplAttributes):Void;
	/**
	 * Returns this  instance as a .
	 * @return A string containing the name, attributes, and exceptions of this
	 * constructor, followed by the current Microsoft intermediate language (MSIL)
	 * stream.
	 */
	function ToString():String;
}
