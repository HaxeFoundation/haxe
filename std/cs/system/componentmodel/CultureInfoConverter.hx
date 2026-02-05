package cs.system.componentmodel;

/** Provides a type converter to convert  objects to and from various other representations. */
@:native("System.ComponentModel.CultureInfoConverter")
extern class CultureInfoConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Gets a value indicating whether this converter can convert an object in the
	 * given source type to a  using the specified context.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type you wish to convert from.
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
	 * Converts the specified value object to a .
	 * @param context An  that provides a format context.
	 * @param culture A  that specifies the culture to which to convert.
	 * @param value The  to convert.
	 * @return An  that represents the converted value.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the given value object to the specified destination type.
	 * @param context An  that provides a format context.
	 * @param culture A  that specifies the culture to which to convert.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the value to.
	 * @return An  that represents the converted .
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Gets a collection of standard values for a  object using the specified context.
	 * @param context An  that provides a format context.
	 * @return A  containing a standard set of valid values, or  if the data type does
	 * not support a standard set of values.
	 */
	function GetStandardValues(context:cs.system.componentmodel.ITypeDescriptorContext):cs.system.componentmodel.TypeConverter_StandardValuesCollection;
	/**
	 * Gets a value indicating whether the list of standard values returned from  is an
	 * exhaustive list.
	 * @param context An  that provides a format context.
	 * @return because the  returned from  is not an exhaustive list of possible values
	 * (that is, other values are possible). This method never returns .
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
