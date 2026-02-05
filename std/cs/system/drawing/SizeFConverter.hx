package cs.system.drawing;

/** Converts  objects from one type to another. */
@:native("System.Drawing.SizeFConverter")
extern class SizeFConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Returns a value indicating whether the converter can convert from the type
	 * specified to the  type, using the specified context.
	 * @param context An  through which additional context can be supplied.
	 * @param sourceType A  the represents the type you wish to convert from.
	 * @return to indicate the conversion can be performed; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Returns a value indicating whether the  can convert a  to the specified type.
	 * @param context An  through which additional context can be supplied.
	 * @param destinationType A  that represents the type you want to convert from.
	 * @return if this converter can perform the conversion otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the given object to the type of this converter, using the specified
	 * context and culture information.
	 * @param context An  that provides a format context.
	 * @param culture The  to use as the current culture.
	 * @param value The  to convert.
	 * @return An  that represents the converted value.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the given value object to the specified type, using the specified
	 * context and culture information.
	 * @param context An  that provides a format context.
	 * @param culture A . If null is passed, the current culture is assumed.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the value parameter to.
	 * @return An  that represents the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Creates an instance of a  with the specified property values using the specified
	 * context.
	 * @param context An  through which additional context can be supplied.
	 * @param propertyValues An  containing property names and values.
	 * @return An  representing the new , or  if the object cannot be created.
	 */
	function CreateInstance(context:cs.system.componentmodel.ITypeDescriptorContext, propertyValues:cs.system.collections.IDictionary):Dynamic;
	/**
	 * Returns a value indicating whether changing a value on this object requires a
	 * call to the  method to create a new value.
	 * @param context An  that provides a format context. This may be .
	 * @return Always returns .
	 */
	function GetCreateInstanceSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Retrieves a set of properties for the  type using the specified context and
	 * attributes.
	 * @param context An  through which additional context can be supplied.
	 * @param value The  to return properties for.
	 * @param attributes An array of  objects that describe the properties.
	 * @return A  containing the properties.
	 */
	function GetProperties(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Returns whether the  type supports properties.
	 * @param context An  through which additional context can be supplied.
	 * @return Always returns .
	 */
	function GetPropertiesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
