package cs.system.componentmodel;

/** Provides a type converter that can be used to populate a list box with available types. */
@:native("System.ComponentModel.TypeListConverter")
extern class TypeListConverter extends cs.system.componentmodel.TypeConverter {
	/**
	 * Gets a value indicating whether this converter can convert the specified  of the
	 * source object using the given context.
	 * @param context An  that provides a format context.
	 * @param sourceType The  of the source object.
	 * @return if this converter can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Gets a value indicating whether this converter can convert an object to the
	 * given destination type using the context.
	 * @param context An  that provides a format context.
	 * @param destinationType A  that represents the type you wish to convert to.
	 * @return if this converter can perform the conversion; otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the specified object to the native type of the converter.
	 * @param context An  that provides a format context.
	 * @param culture A  that specifies the culture used to represent the font.
	 * @param value The  to convert.
	 * @return An  that represents the converted value.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the given value object to the specified destination type.
	 * @param context An  that provides a format context.
	 * @param culture An optional . If not supplied, the current culture is assumed.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the value to.
	 * @return An  that represents the converted .
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Gets a collection of standard values for the data type this validator is
	 * designed for.
	 * @param context An  that provides a format context.
	 * @return A  that holds a standard set of valid values, or  if the data type does
	 * not support a standard set of values.
	 */
	function GetStandardValues(context:cs.system.componentmodel.ITypeDescriptorContext):cs.system.componentmodel.TypeConverter_StandardValuesCollection;
	/**
	 * Gets a value indicating whether the list of standard values returned from the 
	 * method is an exclusive list.
	 * @param context An  that provides a format context.
	 * @return because the  returned from  is an exhaustive list of possible values.
	 * This method never returns .
	 */
	function GetStandardValuesExclusive(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Gets a value indicating whether this object supports a standard set of values
	 * that can be picked from a list using the specified context.
	 * @param context An  that provides a format context.
	 * @return because  should be called to find a common set of values the object
	 * supports. This method never returns .
	 */
	function GetStandardValuesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
