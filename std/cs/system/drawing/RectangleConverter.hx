package cs.system.drawing;

/** Converts rectangles from one data type to another. Access this class through the . */
@:native("System.Drawing.RectangleConverter")
extern class RectangleConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Determines if this converter can convert an object in the given source type to
	 * the native type of the converter.
	 * @param context A formatter context. This object can be used to get additional
	 * information about the environment this converter is being called from. This may
	 * be , so you should always check. Also, properties on the context object may also
	 * return .
	 * @param sourceType The type you want to convert from.
	 * @return This method returns  if this object can perform the conversion;
	 * otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Gets a value indicating whether this converter can convert an object to the
	 * given destination type using the context.
	 * @param context An  object that provides a format context. This can be , so you
	 * should always check. Also, properties on the context object can also return .
	 * @param destinationType A  object that represents the type you want to convert
	 * to.
	 * @return This method returns  if this converter can perform the conversion;
	 * otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the given object to a  object.
	 * @param context A  that can be used to get additional information about the
	 * environment this converter is being called from. This may be , so you should
	 * always check. Also, properties on the context object may also return .
	 * @param culture An  that contains culture specific information, such as the
	 * language, calendar, and cultural conventions associated with a specific culture.
	 * It is based on the RFC 1766 standard.
	 * @param value The object to convert.
	 * @return The converted object.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the specified object to the specified type.
	 * @param context A  that can be used to get additional information about the
	 * environment this converter is being called from. This may be , so you should
	 * always check. Also, properties on the context object may also return .
	 * @param culture An  that contains culture specific information, such as the
	 * language, calendar, and cultural conventions associated with a specific culture.
	 * It is based on the RFC 1766 standard.
	 * @param value The object to convert.
	 * @param destinationType The type to convert the object to.
	 * @return The converted object.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Creates an instance of this type given a set of property values for the object.
	 * This is useful for objects that are immutable but still want to provide
	 * changeable properties.
	 * @param context A  through which additional context can be provided.
	 * @param propertyValues A dictionary of new property values. The dictionary
	 * contains a series of name-value pairs, one for each property returned from a
	 * call to the  method.
	 * @return The newly created object, or  if the object could not be created. The
	 * default implementation returns .
	 */
	function CreateInstance(context:cs.system.componentmodel.ITypeDescriptorContext, propertyValues:cs.system.collections.IDictionary):Dynamic;
	/**
	 * Determines if changing a value on this object should require a call to  to
	 * create a new value.
	 * @param context A type descriptor through which additional context can be
	 * provided.
	 * @return This method returns  if  should be called when a change is made to one
	 * or more properties of this object; otherwise, .
	 */
	function GetCreateInstanceSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Retrieves the set of properties for this type. By default, a type does not
	 * return any properties.
	 * @param context A  through which additional context can be provided.
	 * @param value The value of the object to get the properties for.
	 * @param attributes An array of  objects that describe the properties.
	 * @return The set of properties that should be exposed for this data type. If no
	 * properties should be exposed, this may return . The default implementation
	 * always returns .
	 */
	function GetProperties(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Determines if this object supports properties. By default, this is .
	 * @param context A  through which additional context can be provided.
	 * @return This method returns  if  should be called to find the properties of this
	 * object; otherwise, .
	 */
	function GetPropertiesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
