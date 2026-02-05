package cs.system.componentmodel;

/** Provides automatic conversion between a nullable type and its underlying primitive type. */
@:native("System.ComponentModel.NullableConverter")
extern class NullableConverter extends cs.system.componentmodel.TypeConverter {
	/**
	 * Gets the nullable type.
	 * @return A  that represents the nullable type.
	 */
	var NullableType(default, never):cs.system.Type;
	/**
	 * Gets the underlying type.
	 * @return A  that represents the underlying type.
	 */
	var UnderlyingType(default, never):cs.system.Type;
	/**
	 * Gets the underlying type converter.
	 * @return A  that represents the underlying type converter.
	 */
	var UnderlyingTypeConverter(default, never):cs.system.componentmodel.TypeConverter;
	function new(type:cs.system.Type):Void;
	/**
	 * Returns whether this converter can convert an object of the given type to the
	 * type of this converter, using the specified context.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type you want to convert from.
	 * @return if this converter can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Returns whether this converter can convert the object to the specified type,
	 * using the specified context.
	 * @param context An  that provides a format context.
	 * @param destinationType A  that represents the type you want to convert to.
	 * @return if this converter can perform the conversion; otherwise, .
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
	 * @param culture The  to use as the current culture.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the value parameter to.
	 * @return An  that represents the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Creates an instance of the type that this  is associated with, using the
	 * specified context, given a set of property values for the object.
	 * @param context An  that provides a format context.
	 * @param propertyValues An  of new property values.
	 * @return An  representing the given , or  if the object cannot be created. This
	 * method always returns .
	 */
	function CreateInstance(context:cs.system.componentmodel.ITypeDescriptorContext, propertyValues:cs.system.collections.IDictionary):Dynamic;
	/**
	 * Returns whether changing a value on this object requires a call to  to create a
	 * new value, using the specified context.
	 * @param context An  that provides a format context.
	 * @return if changing a property on this object requires a call to  to create a
	 * new value; otherwise, .
	 */
	function GetCreateInstanceSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Returns a collection of properties for the type of array specified by the value
	 * parameter, using the specified context and attributes.
	 * @param context An  that provides a format context.
	 * @param value An  that specifies the type of array for which to get properties.
	 * @param attributes An array of type  that is used as a filter.
	 * @return A  with the properties that are exposed for this data type, or  if there
	 * are no properties.
	 */
	function GetProperties(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Returns whether this object supports properties, using the specified context.
	 * @param context An  that provides a format context.
	 * @return if  should be called to find the properties of this object; otherwise, .
	 */
	function GetPropertiesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Returns a collection of standard values for the data type this type converter is
	 * designed for when provided with a format context.
	 * @param context An  that provides a format context that can be used to extract
	 * additional information about the environment from which this converter is
	 * invoked. This parameter or properties of this parameter can be .
	 * @return A  that holds a standard set of valid values, or  if the data type does
	 * not support a standard set of values.
	 */
	function GetStandardValues(context:cs.system.componentmodel.ITypeDescriptorContext):cs.system.componentmodel.TypeConverter_StandardValuesCollection;
	/**
	 * Returns whether the collection of standard values returned from  is an exclusive
	 * list of possible values, using the specified context.
	 * @param context An  that provides a format context.
	 * @return if the  returned from  is an exhaustive list of possible values;  if
	 * other values are possible.
	 */
	function GetStandardValuesExclusive(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Returns whether this object supports a standard set of values that can be picked
	 * from a list, using the specified context.
	 * @param context An  that provides a format context.
	 * @return if  should be called to find a common set of values the object supports;
	 * otherwise, .
	 */
	function GetStandardValuesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Returns whether the given value object is valid for this type and for the
	 * specified context.
	 * @param context An  that provides a format context.
	 * @param value The  to test for validity.
	 * @return if the specified value is valid for this object; otherwise, .
	 */
	function IsValid(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic):Bool;
}
