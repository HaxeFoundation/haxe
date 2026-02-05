package cs.system.componentmodel;

/** Provides a type converter to convert  objects to and from various other representations. */
@:native("System.ComponentModel.EnumConverter")
extern class EnumConverter extends cs.system.componentmodel.TypeConverter {
	/**
	 * Gets an  that can be used to sort the values of the enumeration.
	 * @return An  for sorting the enumeration values.
	 */
	var Comparer(default, never):cs.system.collections.IComparer;
	/**
	 * Specifies the type of the enumerator this converter is associated with.
	 * @return The type of the enumerator this converter is associated with.
	 */
	var EnumType(default, never):cs.system.Type;
	/**
	 * Gets or sets a  that specifies the possible values for the enumeration.
	 * @return A  that specifies the possible values for the enumeration.
	 */
	var Values(default, default):cs.system.componentmodel.TypeConverter_StandardValuesCollection;
	function new(type:cs.system.Type):Void;
	/**
	 * Gets a value indicating whether this converter can convert an object in the
	 * given source type to an enumeration object using the specified context.
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
	 * Converts the specified value object to an enumeration object.
	 * @param context An  that provides a format context.
	 * @param culture An optional . If not supplied, the current culture is assumed.
	 * @param value The  to convert.
	 * @return An  that represents the converted .
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
	 * Gets a value indicating whether the list of standard values returned from  is an
	 * exclusive list using the specified context.
	 * @param context An  that provides a format context.
	 * @return if the  returned from  is an exhaustive list of possible values;  if
	 * other values are possible.
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
	/**
	 * Gets a value indicating whether the given object value is valid for this type.
	 * @param context An  that provides a format context.
	 * @param value The  to test.
	 * @return if the specified value is valid for this object; otherwise, .
	 */
	function IsValid(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic):Bool;
}
