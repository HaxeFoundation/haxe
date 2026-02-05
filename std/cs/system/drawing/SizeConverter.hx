package cs.system.drawing;

/** The  class is used to convert from one data type to another. Access this class through the  object. */
@:native("System.Drawing.SizeConverter")
extern class SizeConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Determines whether this converter can convert an object in the specified source
	 * type to the native type of the converter.
	 * @param context A  that can be used to get additional information about the
	 * environment this converter is being called from. This may be , so you should
	 * always check. Also, properties on the context object may also return .
	 * @param sourceType The type you want to convert from.
	 * @return This method returns  if this object can perform the conversion.
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Gets a value indicating whether this converter can convert an object to the
	 * given destination type using the context.
	 * @param context A  that can be used to get additional information about the
	 * environment this converter is being called from. This can be , so always check.
	 * Also, properties on the context object can return .
	 * @param destinationType A  that represents the type you want to convert to.
	 * @return This method returns  if this converter can perform the conversion;
	 * otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the specified object to the converter's native type.
	 * @param context A  that can be used to get additional information about the
	 * environment this converter is being called from. This may be , so you should
	 * always check. Also, properties on the context object may also return .
	 * @param culture An  object that contains culture specific information, such as
	 * the language, calendar, and cultural conventions associated with a specific
	 * culture. It is based on the RFC 1766 standard.
	 * @param value The object to convert.
	 * @return The converted object.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the specified object to the specified type.
	 * @param context A  that can be used to get additional information about the
	 * environment this converter is being called from. This may be , so you should
	 * always check. Also, properties on the context object may also return .
	 * @param culture An  object that contains culture specific information, such as
	 * the language, calendar, and cultural conventions associated with a specific
	 * culture. It is based on the RFC 1766 standard.
	 * @param value The object to convert.
	 * @param destinationType The type to convert the object to.
	 * @return The converted object.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Creates an object of this type by using a specified set of property values for
	 * the object. This is useful for creating non-changeable objects that have
	 * changeable properties.
	 * @param context A  through which additional context can be provided.
	 * @param propertyValues A dictionary of new property values. The dictionary
	 * contains a series of name-value pairs, one for each property returned from the 
	 * method.
	 * @return The newly created object, or  if the object could not be created. The
	 * default implementation returns .
	 */
	function CreateInstance(context:cs.system.componentmodel.ITypeDescriptorContext, propertyValues:cs.system.collections.IDictionary):Dynamic;
	/**
	 * Determines whether changing a value on this object should require a call to the 
	 * method to create a new value.
	 * @param context A  through which additional context can be provided.
	 * @return if the  object should be called when a change is made to one or more
	 * properties of this object.
	 */
	function GetCreateInstanceSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Retrieves the set of properties for this type. By default, a type does not have
	 * any properties to return.
	 * @param context A  through which additional context can be provided.
	 * @param value The value of the object to get the properties for.
	 * @param attributes An array of  objects that describe the properties.
	 * @return The set of properties that should be exposed for this data type. If no
	 * properties should be exposed, this may return . The default implementation
	 * always returns .
	 */
	function GetProperties(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Determines whether this object supports properties. By default, this is .
	 * @param context A  through which additional context can be provided.
	 * @return if the  method should be called to find the properties of this object.
	 */
	function GetPropertiesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
